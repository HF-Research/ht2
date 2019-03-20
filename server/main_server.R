options(DT.options = list(
  pageLength = 20,
  dom = "Bt",
  buttons = c('copy', 'csv', 'pdf')
))

# UPDATE RADIO BUTTONS ----------------------------------------------------
output$varButtonChoices <- renderUI({
  # Gives a dynamic button UI. The buttons change depending on the selected
  # outcome Keep variables that have "count" in their name.
  #
  # When page loads, this UI element initally returns NULL to server fn(),
  # then it re-runs and returns the initial value - eg. "age". This means we
  # have to restrict the output that depends on this (which is nearly
  # everything) from running until a non-NULL value is supplied. This is
  # acheived by an if-statement in the validate() reactive.
  
  var_names <-
    grep("count", names(subsetOutcomeWithoutAggreLevel()), value = TRUE)
  variable_choices <-
    variable_ui[code_name %in% var_names, .(code_name, var_dk)]
  var_names <- variable_choices$code_name
  names(var_names) <- variable_choices$var_dk
  selectInput(
    inputId = "variable",
    label = choose_var,
    choices = var_names,
    selectize = TRUE
  )
  # radioGroupButtons(
  #   inputId = "variable",
  #   label = choose_var,
  #   choices = var_names,
  #   justified = TRUE,
  #   direction = "vertical",
  #   individual = FALSE
  # )
})


output$downloadButton <- renderUI({
  if (input$aggr_level != "national") {
    actionBttn(inputId = "download_bar",
               label = "Download",
               size = "sm")
  } else if (input$aggr_level == "national") {
    actionBttn(inputId = "download_line",
               label = "Download",
               size = "sm")
  }
  
})

# TEXT RENDERING ----------------------------------------------------------
output$outcome_title <- renderText({
  input$outcome
})
output$outcome_description <- renderUI({
  out <-
    outcome_descriptions[hjertetal_code == outcomeCode(), .(desc_dk, link_dk)]
  # Add link for further reading - if link exists, otherwise just desc
  url <- a(ui_read_more,
           href = (out$link_dk),
           target = "_blank")
  
  if (out$link_dk != "na") {
    tagList(out$desc_dk, url)
  }
  else {
    tagList(out$desc_dk)
  }
})

output$variable_desc <- renderUI({
  # Append title to front of variable descr text
  if (validate()) {
    title_text <- tags$b(prettyVariable()[1])
    
    col_selection <- paste0("desc_general_", lang)
    desc_text <-
      variable_ui[code_name == selectedDataVars()[1], get(col_selection)]
    tagList(title_text, desc_text)
  }
})

plotTitle <- reactive({
  if (validate())
    paste0(prettyVariable()[1], " - ", input$outcome)
})

output$rate_count_desc <- renderUI({
  if (validate()) {
    if (input$count_rates == 2) {
      title_text <- tags$b(prettyVariable()[2])
      col_selection <-
        paste0("desc_", selectedRateType(), "_", lang)
      desc_text <-
        variable_ui[code_name == selectedDataVars()[1], get(col_selection)]
      tagList(title_text, desc_text)
    } else {
      title_text <-
        tags$b(paste0(ui_count_rate[1], " ", tolower(prettyVariable()[1])))
      
      col_selection <- paste0("desc_", "count", "_", lang)
      desc_text <-
        variable_ui[code_name == selectedDataVars()[1], get(col_selection)]
      tagList(title_text, desc_text)
    }
  }
})

output$table1_title <- renderText({
  if (validate())
    paste0(prettyVariable()[1], ": ", ui_count_rate[1])
})

output$table2_title <- renderText({
  if (validate())
    prettyVariable()[2]
})



# DYNAMIC VARIABLES/COLUMN NAMES ------------------------------------------

outcomeCode <- reactive({
  # Connect the input in normal language to the hjertetal_code. This is so we
  # can change the description without having to rename allll the datasets.
  
  outcomes_all[name_dk == input$outcome, hjertetal_code]
})


outcomeGroup <- reactive({
  # Define which type of outcome are in the outputed dataset.
  if (any(outcome_names_treatment$hjertetal_code %in% outcomeCode())) {
    outcome_group <- "treatment"
  } else if (any(outcome_names_diag$hjertetal_code %in% input$outcome)) {
    outcome_group <- "diag"
  } else if (any(out_names_med %in% input$outcome)) {
    outcome_group <- "med"
  }
  outcome_group
})

prettyAggr_level <- reactive({
  # Outputs same character string that's used in the UI input field
  names(which(aggr_choices == input$aggr_level))
})

prettyVariable <- reactive({
  # Outputs character string formatted for user.
  data_var_names <- selectedDataVars()
  
  grep_selection <-
    paste0("var_rate_", selectedRateType(), "_", lang)
  col_names <- colnames(variable_ui)
  col_selection <- grep(grep_selection, col_names, value = TRUE)
  c(variable_ui[code_name == data_var_names[1], var_dk], variable_ui[code_name == data_var_names[1], get(col_selection)])
  
})


# SUBSETTING ------------------------------------------------------
selectCountRate <- function() {
  as.integer(input$count_rates)
}

subsetOutcomeWithoutAggreLevel <- reactive({
  # Some functions should not depend on the state of input$aggr_level - but
  # still need to know which variables are available to the user. More
  # specifically, I do not want some functions being re-run everytime aggr_level
  # changes (i.e. updating the available variabls - this resets the variable
  # n_incidence everytime aggr_level changes).
  #
  # However, this means I cannot change the variables available to the user when
  # switching between agg_levels (obviously).
  shiny_dat[[outcomeCode()]]$age
})

subsetOutcome <- reactive({
  # Cache subset based on outcome, aggr level, and theme
  if (input$aggr_level != "national") {
    shiny_dat[[outcomeCode()]][[input$aggr_level]]
  } else {
    # Age is needed because it is the only aggregation subset that includes all
    # the data
    subsetOutcomeWithoutAggreLevel()
  }
  
})

selectedRateType <- reactive({
  if (input$aggr_level == "age") {
    "stratified"
  } else {
    "standardized"
  }
})
selectedDataVars <- function() {
  var_stripped <- gsub("count_|rate_", "", input$variable)
  grep_str <- paste0(var_stripped, "$")
  grep(grep_str, colnames(subsetOutcome()), value = TRUE)
}

subsetVars <- reactive({
  dat <- subsetOutcome()
  if (input$aggr_level != "national") {
    col_vars <- c("year", "sex", "grouping", selectedDataVars())
    dat <- dat[, ..col_vars]
    colnames(dat) <-
      c(ui_year, ui_sex, prettyAggr_level(), prettyVariable())
  } else {
    col_vars <- c("year", "sex", selectedDataVars())
    dat <- dat[, ..col_vars]
    colnames(dat) <-
      c(ui_year, ui_sex, prettyVariable())
  }
  dat[]
})
subsetYear <- function()
  ({
    # Subset the already partially subset data based on years
    
    dat <- subsetVars()[get(ui_year) == input$year,]
    dat[]
  })


# FORMATTING DATA FOR D3------------------------------------------------------
outputCasesData <- reactive({
  # National level data shows all years
  if (input$aggr_level != "national") {
    dat <- subsetYear()
    dat[, (ui_year) := NULL]
  } else {
    subsetVars()
  }
})

outputCasesD3Line <- reactive({
  # Replace value.var with reactive that corresponds to the variable the user selected
  dat <-
    dcast(
      subsetVars(),
      get(ui_year) ~ get(ui_sex),
      value.var = prettyVariable()[selectCountRate()],
      fun.aggregate = sum
    )
  
  colnames(dat) <-
    c(ui_year, "female", "male") # TODO: needs to be language agnostic
  dat[, variable := prettyVariable()[selectCountRate()]]
  
})

outputCasesD3Bar <- reactive({
  # Restrict data to the user selected vairable, and give pretty column names
  count_rate <- prettyVariable()[selectCountRate()]
  keep_cols <- c(ui_sex, prettyAggr_level(), count_rate)
  dat <- subsetYear()[, ..keep_cols]
  dat <- dat[, (count_rate) := lapply(.SD, function(i) {
    # Any NA values need to be converted to 0s to be sent to d3
    i[is.na(i)] <- 0
    i
  }),
  .SDcols = count_rate]
  
  # Order so that males come first - makes sure the coloring matches
  dat[order(-get(ui_sex)),]
  
})


plot_d3_bar <- reactive({
  if (nrow(outputCasesD3Bar()) > 0  &
      input$aggr_level != "national") {
    sex_vars <- ui_sex_levels
    color = c("#bd6916", "#166abd")
    plot_title = plotTitle()
    simpleD3Bar(
      data = outputCasesD3Bar(),
      colors = c("#bd6916", "#166abd"),
      plotTitle = plot_title,
      sexVars = sex_vars
    )
  }
  
})


plot_d3_line <- reactive({
  if (input$aggr_level == "national") {
    sex_vars <- ui_sex_levels
    color = c("#bd6916", "#166abd")
    
    plot_title = plotTitle()
    simpleD3Line(
      data = outputCasesD3Line(),
      colors = c("#bd6916", "#166abd"),
      plotTitle = plot_title,
      sexVars = sex_vars
    )
  }
})



# DATATABLES --------------------------------------------------------------
outputCountDTTable <- reactive({
  # Organizes data for DataTable outputs. Needs to be characters
  dat <- copy(outputCasesData())
  group_var <- prettyAggr_level()
  dat[, prettyVariable()[2] := NULL]
  
  dat <-  dcast(
    dat,
    get(group_var) ~ get(ui_sex),
    value.var = prettyVariable()[1],
    fun.aggregate = sum
  )
  
  # Calculate margins
  dat[, Total := rowSums(dat[, .(female, male)], na.rm = TRUE)]
  if (input$aggr_level == "age") {
    totals <-
      dat[, colSums(dat[, .(female, male, Total)], na.rm = TRUE)]
    
    # Convert entire table to character to we can rbind() totals
    dat <- dat[, lapply(.SD, as.character)]
    # Rbind totals
    dat <- rbindlist(list(dat, as.list(c("Total", totals))))
  } else {
    dat <- dat[, lapply(.SD, as.character)]
  }
  # Format data columns to either DK or EN settings
  dat <-
    formatNumbers(dat)
  
  
  # .SDcols = col_names]]
  colnames(dat) <- c(group_var, ui_sex_levels, "Total")
  #
  # Flag last row so can be targeted for formatting
  dat[, flag := 0]
  if (input$aggr_level == "age")
    dat[nrow(dat), flag := 1]
  # Make sure "flag" variable is always first column, so we can
  # reference by col index in formatting fn()
  col_names <- colnames(dat)
  col_names <-
    c(col_names[length(col_names)], col_names[-length(col_names)])
  setcolorder(dat, neworder = col_names)
  
  makeCountDT(dat, group_var = group_var)
  
})

outputRateDTTable <- reactive({
  dat <- copy(outputCasesData())
  group_var <- prettyAggr_level()
  dat[, prettyVariable()[1] := NULL]
  
  dat <-  dcast(
    dat,
    get(group_var) ~ get(ui_sex),
    value.var = prettyVariable()[2],
    fun.aggregate = sum
  )
  
  # Format data columns in either DK or EN numbers
  dat <-
    formatNumbers(dat)
  
  colnames(dat) <- c(group_var, ui_sex_levels)
  makeRateDT(dat = dat, group_var = group_var)
})

# VALIDATE BEFORE PLOTING -------------------------------------------------
validate <- reactive({
  # Returns TRUE if passes and FALSE if any condition fails. This is needed to
  # stop the plots and tables trying to render when they have inproper input.
  # I.e. when switching between outcomes, the variable inupt is -
  #
  nonZero_variable <- !is.null(input$variable)
  if (nonZero_variable) {
    length(selectedDataVars()) > 0 && input$outcome != "" &&
      input$year > 0 && input$year != "" && validateKom()
  } else {
    FALSE
  }
})

validateKom <- reactive({
  # If geographic aggregation level is selected, the selected year must be >=
  # 2009. This invalid combination cannot be directly selected by the user, but
  # is created during an intermediate step in Shiny, before the "year" selected
  # is reset. This validate step stops Shiny from processing the data steps -
  # and thus throwing an error - during this intermediate step.
  if (input$aggr_level == "kom") {
    input$year >= 2009
  } else if (input$aggr_level != "kom") {
    TRUE
  } else {
    FALSE
  }
})

# CHANGE UI BASED ON INPUTS -----------------------------------------------

choiceYears <- reactive({
  # The following additional if-else logic is needed to stop the year count
  # always resetting to 2015 when changing aggr_level.
  if (input$year != "") {
    selected_year <- input$year
  } else {
    selected_year <- "2015"
  }
  
  # Set year-range to be used by udateSelectInput()
  if (input$aggr_level == "kom") {
    year_range <- c(2009:2015)
    if (input$year < 2009)
      selected_year <- "2015"
  } else {
    year_range <- c(2006:2015)
  }
  return(list(selected_year = selected_year,
              year_range = year_range))
  
})
observe({
  # User can only select years >=2009 when viewing regional data.
  updateSelectInput(
    session = session,
    inputId = "year",
    choices = choiceYears()$year_range,
    selected = choiceYears()$selected_year
  )
})

observe({
  # Disable "year" when showing longitudinal data
  shinyjs::toggleState(id = "year",
                       condition = input$aggr_level != "national")
  
})




# RENDER FUNCTIONS --------------------------------------------------------

# PLOT
output$d3_plot_bar <- renderSimpleD3Bar({
  if (validate() & input$aggr_level != "national") {
    plot_d3_bar()
  }
})

output$d3_plot_line_html <- renderSimpleD3Line({
  if (validate()) {
    plot_d3_line()
  }
  
})


# DATATABLES:
# AGE
output$table <- renderDT({
  server = TRUE
  if (validate()) {
    outputCountDTTable()
  }
})

output$table_margins <- renderDT({
  if (validate()) {
    outputRateDTTable()
  }
})
