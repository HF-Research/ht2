options(DT.options = list(
  pageLength = 20,
  dom = "Bt",
  buttons = c('copy', 'csv', 'pdf')
))
callModule(profvis_server, "profiler")
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
  var_names <- var_names[!grepl("mean", var_names)]
  
  # Select the plain language terms matching the variables in data
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
               label = "Hente figure",
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
      variable_ui[code_name == selectedDataVars()[1], ..col_selection]
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
        variable_ui[code_name == selectedDataVars()[1], ..col_selection]
      tagList(title_text, desc_text)
    } else {
      title_text <-
        tags$b(paste0(ui_count_rate[1], " ", tolower(prettyVariable()[1])))
      
      col_selection <- paste0("desc_", "count", "_", lang)
      desc_text <-
        paste0(variable_ui[code_name == selectedDataVars()[1], ..col_selection])
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
  data_var_name <- selectedDataVars()[1]
  
  grep_selection <-
    paste0("var_rate_", selectedRateType(), "_", lang)
  col_names <- colnames(variable_ui)
  col_selection <- grep(grep_selection, col_names, value = TRUE)
  c(variable_ui[code_name == data_var_name, var_dk], paste0(variable_ui[code_name == data_var_name, ..col_selection]))
  
})

prettyVariableSingular <- reactive({
  prettyVariable()[selectCountRate()]
})


# SUBSETTING ------------------------------------------------------
selectCountRate <- reactive({
  as.integer(input$count_rates)
})

selectRawOrMean <- reactive({
  # Returns TRUE if raw count data should be used. FALSE if moving avg data
  # should be used
  if (input$aggr_level %in% c("age", "national")) {
    TRUE
  } else if (input$aggr_level %in% c("edu", "region", "kom")) {
    FALSE
  }
})

selectPercentOrRate <- reactive({
  if (input$variable %in% c(
    "count_n_readmissions_ppl_30",
    "count_n_dead30",
    "count_n_dead1",
    "count_n_dead5"
  )) {
    TRUE
  } else {
    FALSE
  }
  
})

selectGeo <- function() {
  if (input$aggr_level %in% c("kom", "region")) {
    TRUE
  } else {
    FALSE
  }
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
selectedDataVars <- function(){
  # Returns the column names to be used to subset the data - taking into account
  # raw or mean data
  var_stripped <- gsub("count_|rate_", "", input$variable)
  grep_str <- paste0(var_stripped, "$")
  grep(grep_str, colnames(subsetOutcome()), value = TRUE)
}

subsetVars <- function(){
  dat <- subsetOutcome()
  
  # Switch between RAW and MOVNIG AVG data
  data_vars <- selectedDataVars()
  if (selectRawOrMean()) {
    data_vars <- data_vars[!grepl("mean", data_vars)]
  } else {
    data_vars <- data_vars[grepl("mean", data_vars)]
  }
  
  # Select based on aggre_level
  if (input$aggr_level != "national") {
    col_vars <- c("year", "sex", "grouping", data_vars)
    dat <- dat[, ..col_vars]
    colnames(dat) <-
      c(ui_year, ui_sex, prettyAggr_level(), prettyVariable())
  } else {
    col_vars <- c("year", "sex", data_vars)
    dat <- dat[, ..col_vars]
    colnames(dat) <-
      c(ui_year, ui_sex, prettyVariable())
  }
  dat[]
}
subsetYear <- function(){
  # Subset the already partially subset data based on years
  
  dat <- subsetVars()[get(ui_year) == input$year,]
  if (selectPercentOrRate()) {
    var_to_modify <- grep(ui_percent, names(dat), value = TRUE)
    dat[, (var_to_modify) := round(get(var_to_modify) / 1000, digits = 1)]
  }
  dat[]
}


# FORMATTING DATA FOR D3------------------------------------------------------
outputCasesData <- function(){
  # National level data shows all years
  if (input$aggr_level != "national") {
    sub_year <- subsetYear()
    sub_year[, (ui_year) := NULL]
  } else {
    subsetVars()
  }
}

outputCasesD3Line <- reactive({
  # Replace value.var with reactive that corresponds to the variable the user selected
  cast_formula <- formula(paste0(ui_year, "~", ui_sex))
  tmp <- subsetVars()
  value_var <- prettyVariableSingular()
  
  dat <-
    dcast(
      tmp,
      cast_formula,
      value.var = value_var,
      fun.aggregate = function(x) sum(x, na.rm = TRUE)
    )
  
  colnames(dat) <-
    c(ui_year, "female", "male") # TODO: needs to be language agnostic
  dat[, variable := value_var]
  
})

outputCasesD3Bar <- reactive({
  # Restrict data to the user selected vairable, and give pretty column names
  count_rate <- prettyVariableSingular()
  keep_cols <- c(ui_sex, prettyAggr_level(), count_rate)
  dat <- subsetYear()[, ..keep_cols]
  dat <- dat[, (count_rate) := lapply(.SD, function(i) {
    # Any NA values need to be converted to 0s to be sent to d3
    i[is.na(i)] <- 0
    i
  }),
  .SDcols = count_rate]
  
  # For variables that present PERCENTAGE results - divide by 1000
  
  # Order so that males come first - makes sure the coloring matches
  setorderv(dat, ui_sex, order = -1L)
  
  # For kommune data re-order based on rate or count
  
  if (input$aggr_level == "kom") {
    setorderv(dat, c(ui_sex, count_rate), order = -1L)
  }
  dat[]
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



# LEAFLET MAPS ------------------------------------------------------

mapObj <- function(){
  if (input$aggr_level == "kom") {
    dk_sp$l2
  } else if (input$aggr_level == "region") {
    dk_sp$l1
  }
}

mapData <- function() {
  out <- mapObj()
  data_var <- prettyVariableSingular()
  keep_vars <- c("id", prettyAggr_level(), data_var)
  
  tmp <- copy(outputCasesD3Bar())
  # inx <- duplicated(tmp[, ..data_var])
  # # tmp[inx, (data_var) := jitter(get(data_var), amount = .1)]
  
  # MALES
  tmp <- tmp[get(ui_sex) == "male"]
  setkeyv(tmp, prettyAggr_level())
  out@data <- tmp[out@data, ..keep_vars]
  setorder(out@data, id)
  m <- out
  
  # Female
  out <- mapObj()
  tmp <- outputCasesD3Bar()[get(ui_sex) == "female"]
  setkeyv(tmp, prettyAggr_level())
  out@data <- tmp[out@data, ..keep_vars]
  setorder(out@data, id)
  
  # Combine for output
  list(male = m,
       female = out)
  
}

output$maps <- renderCombineWidgets({
  if (validate() && isGeo()){
    name_lang <- paste0("name_", lang)
  map_data <-  mapData()$male
  popup <- paste0(
    prettyAggr_level(),
    ": <strong>",
    map_data@data[["name_dk"]],
    "</strong><br><br>",
    map_data@data[[prettyVariableSingular()]]
  ) %>%
    lapply(htmltools::HTML)
  
  fill_colors <- ~ pal(map_data@data[[prettyVariableSingular()]])
  map_m <- makeLeaflet(map_data = map_data,
                       fill_colors = fill_colors,
                       label_popup = popup)
  
  # Female
  map_data <-  mapData()$female
  popup <- paste0(
    prettyAggr_level(),
    ": <strong>",
    map_data@data[["name_dk"]],
    "</strong><br><br>",
    map_data@data[[prettyVariableSingular()]]
  ) %>%
    lapply(htmltools::HTML)
  
  fill_colors <- ~ pal(map_data@data[[prettyVariableSingular()]])
  map_f <- makeLeaflet(map_data = map_data,
                       fill_colors = fill_colors,
                       label_popup = popup)
  
  combineWidgets(map_m, map_f, ncol = 2)
  
  }
})




# DATATABLES --------------------------------------------------------------



dtCast <- reactive({
  # One dcast for both rates and counts
  if(validate()){
  group_var <- prettyAggr_level()
  dat <- outputCasesData()
  value_var <- prettyVariable()
  cast_formula <- formula(paste0(group_var, "~", ui_sex))
  out <- dcast(dat,
               cast_formula,
               value.var = value_var,
               fun.aggregate = function(x) sum(x, na.rm = TRUE))
  setnames(out, names(out)[1], "group_var")
  }
})


outputCountDTTable <- reactive({
  if(validate()){
  # Organizes data for DataTable outputs. Needs to be characters
  dat <- dtCast()
  # Subset to either counts or rates
  
  vars <-
    c("group_var", grep(prettyVariable()[1], colnames(dat), value = TRUE))
  
  dat <- dat[, ..vars]
  colnames(dat) <- c("group_var", "female", "male")
  # Calculate margins
  dat[, Total := rowSums(dat[, .(female, male)], na.rm = TRUE)]
  if (input$aggr_level == "age") {
    # Only calculate bottom margins for "age" - other aggr levels don't include
    # full data
    totals <-
      dat[, colSums(dat[, .(female, male, Total)], na.rm = TRUE)]
    
    # Convert entire table to character to we can rbind() totals
    dat <- dat[, lapply(.SD, as.character)]
    # Rbind totals
    dat <- rbindlist(list(dat, as.list(c("Total", totals))))
    
    # Convert back to numeric
    col_convert <- c("female", "male", "Total")
    dat[, (col_convert) := lapply(.SD, as.numeric), .SDcols = col_convert]
    
  }
  
  setnames(
    dat,
    old = c("group_var", "male", "female"),
    new = c(prettyAggr_level(), ui_sex_levels)
  )
  
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
  
  
  
  
  
  makeCountDT(dat,
              group_var = prettyAggr_level(),
              thousands_sep = thousands_sep)
  }
})

outputRateDTTable <- reactive({
  if(validate()){
  dat <- dtCast()
  # Subset to either counts or rates
  vars <-
    c("group_var",
      grep(
        prettyVariable()[2],
        colnames(dat),
        fixed = TRUE,
        value = TRUE
      ))
  dat <- dat[, ..vars]
  colnames(dat) <- c("group_var", "female", "male")
  
  if (!selectPercentOrRate()) {
    digits = 0
  } else {
    digits = 1
  }
  
  setnames(
    dat,
    old = c("group_var", "male", "female"),
    new = c(prettyAggr_level(), ui_sex_levels)
  )
  # colnames(dat) <- c(group_var, ui_sex_levels)
  makeRateDT(
    dat = dat,
    group_var = prettyAggr_level(),
    thousands_sep = thousands_sep,
    digits = digits,
    dec_mark = dec_mark
  )
  }
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

isGeo <- reactive({
  input$aggr_level == "kom" ||
    input$aggr_level == "region"
})

# CHANGE UI BASED ON INPUTS -----------------------------------------------

choiceYears <- reactive({
  # The following additional if-else logic is needed to stop the year count
  # always resetting to 2015 when changing aggr_level.
  if (input$year != "") {
    selected_year <- input$year
  } else {
    selected_year <- year_max
  }
  
  null_var <- is.null(input$variable)
  # Set year-range to be used by udateSelectInput()
  if (selectGeo() &&
      (null_var ||
       input$variable != "count_n_dead5")) {
    year_range <- c(2009:year_max)
    if (input$year < 2009)
      selected_year <- 2009
    
  } else if (selectGeo() &&
             !null_var && input$variable == "count_n_dead5") {
    year_range <- c(2009:(year_max - 4))
    if (input$year < 2009) {
      selected_year <- 2009
    } else if (input$year > (year_max - 4)) {
      selected_year <- year_max - 4
    }
    
  } else if (!selectGeo() &&
             !null_var && input$variable == "count_n_dead5") {
    year_range <- c(2006:(year_max - 4))
    if (input$year > (year_max - 4)) {
      selected_year <- year_max - 4
    }
    
  } else {
    year_range <- c(2006:year_max)
  }
  return(list(selected_year = selected_year,
              year_range = year_range))
  
})
observe({
  # User can only select years >=2009 when viewing regional data and <=2012 when
  # viewing 5-year mortality
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

observe({
  # Shows map tab only when geo data is selected
  shinyjs::toggle(
    condition = (input$aggr_level == "kom" ||
                   input$aggr_level == "region"),
    selector = paste0("#data_vis_tabs li a[data-value=", ui_map, "]")
  )
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
  
  if (validate()) {
    outputCountDTTable()
  }
})

output$table_margins <- renderDT({
  if (validate()) {
    outputRateDTTable()
  }
})
