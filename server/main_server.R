options(DT.options = list(
  pageLength = 20,
  dom = "Bt",
  buttons = c('copy', 'csv', 'pdf')
))
callModule(profvis_server, "profiler")
# TEXT RENDERING ----------------------------------------------------------
output$outcome_title <- renderText({
  input$outcome
})
output$outcome_description <- renderUI({
  req(input$outcome)
  
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
  
  req(input$variable)
  isolate({
    title_text <- tags$b(prettyVariable()[1])
    
    col_selection <- paste0("desc_general_", lang)
    desc_text <-
      variable_ui[code_name == selectedDataVars()[1], ..col_selection]
    tagList(title_text, desc_text)
  })
})

plotTitle <- reactive({
  paste0(prettyVariable()[1], " - ", input$outcome)
})

output$rate_count_desc <- renderUI({
  req(input$count_rates, input$variable)
  isolate({
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
  })
})

output$table1_title <- renderText({
  paste0(prettyVariable()[1], ": ", ui_count_rate[1])
})

output$table2_title <- renderText({
  prettyVariable()[2]
})


output$tabs <- reactive({
  input$data_vis_tabs != ui_data
})

output$tabFigure <- reactive({
  input$data_vis_tabs == ui_d3_figures
})
output$tabMap <- reactive({
  
  input$data_vis_tabs == ui_map && isGeo()
})


outputOptions(output, "tabs", suspendWhenHidden = FALSE)
outputOptions(output, "tabFigure", suspendWhenHidden = FALSE)
outputOptions(output, "tabMap", suspendWhenHidden = FALSE)


# DYNAMIC VARIABLES/COLUMN NAMES ------------------------------------------

outcomeCode <- reactive({
  # Connect the input in normal language to the hjertetal_code. This is so we
  # can change the description without having to rename allll the datasets.
  
  outcomes_all[name_dk == input$outcome, hjertetal_code]
})

prettyAggr_level <- reactive({
  # Outputs same character string that's used in the UI input field
  names(which(aggr_choices == input$aggr_level))
})

prettyVariable <- reactive({
  # Outputs character string formatted for user.
  req(input$variable)
  input$variable
  isolate({
    data_var_name <- selectedDataVars()[1]
    grep_selection <-
      paste0("var_rate_", selectedRateType(), "_", lang)
    col_names <- colnames(variable_ui)
    col_selection <- grep(grep_selection, col_names, value = TRUE)
    c(variable_ui[code_name == data_var_name, var_dk], paste0(variable_ui[code_name == data_var_name, ..col_selection]))
  })
})

prettyVariableSingular <- reactive({
  prettyVariable()[as.integer(input$count_rates)]
})


# SELECTORS ---------------------------------------------------------------
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


# SUBSETTING ------------------------------------------------------
subsetOutcome <- reactive({
  # Cache subset based on outcome, aggr level, and theme
  
  shiny_dat[[outcomeCode()]][[input$aggr_level]]
  
  
})

selectedRateType <- reactive({
  if (input$aggr_level == "age") {
    "stratified"
  } else {
    "standardized"
  }
})
selectedDataVars <- reactive({
  # Returns the column names to be used to subset the data - taking into account
  # raw or mean data
  var_stripped <- gsub("count_|rate_", "", input$variable)
  grep_str <- paste0(var_stripped, "$")
  grep(grep_str, colnames(subsetOutcome()), value = TRUE)
})

subsetVars <- reactive({
  dat <- subsetOutcome()
  
  # Switch between RAW and MOVNIG AVG data
  data_vars <- selectedDataVars()
  if (selectRawOrMean()) {
    data_vars <- data_vars[!grepl("mean", data_vars)]
  } else {
    data_vars <- data_vars[grepl("mean", data_vars)]
  }
  
  col_vars <- c("year", "sex", "grouping", data_vars)
  dat <- dat[, ..col_vars]
  
  # Select based on aggre_level
  if (input$aggr_level != "national") {
    setnames(dat,
             c(ui_year, ui_sex, prettyAggr_level(), prettyVariable()))
  } else {
    setnames(dat, c(ui_year, ui_sex, "age", prettyVariable()))
  }
  
  if (selectPercentOrRate()) {
    var_to_modify <- grep(ui_percent, names(dat), value = TRUE)
    dat[, (var_to_modify) := round(get(var_to_modify) / 1000, digits = 1)]
  }
  
  
  dat[]
})
subsetYear <- reactive({
  # Subset the already partially subset data based on years
  subsetVars()[get(ui_year) == input$year,][, (ui_year) := NULL]
})


# FORMATTING DATA FOR D3------------------------------------------------------
outputCasesData <- function() {
  # National level data shows all years
  if (!isNational()) {
    subsetYear()
  } else {
    subsetVars()
  }
}

outputCasesD3Line <- reactive({
  # Replace value.var with reactive that corresponds to the variable the user selected
  
  dat <- dtCast()
  vars <-
    c("group_var",
      grep(
        prettyVariableSingular(),
        colnames(dat),
        fixed = TRUE,
        # because special characters exits
        value = TRUE
      ))
  dat <- dat[, ..vars]
  setnames(dat, c(ui_year, "female", "male")) # TODO: needs to be language agnostic
  
  # Column containing variable name to send to D3. TODO: send this data as
  # single data point - so change d3 widget
  dat[, variable := prettyVariableSingular()]
  
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
  dat[]
})


plot_d3_bar <- reactive({
  if (nrow(outputCasesD3Bar()) > 0  &&
      !isNational()) {
    sex_vars <- ui_sex_levels
    color = c("#bd6916", "#166abd")
    plot_title = plotTitle()
    
    # For kommune data re-order based on rate or count
    dat <- copy(outputCasesD3Bar())
    if (input$aggr_level == "kom") {
      setorderv(dat, c(ui_sex, prettyVariableSingular()), order = -1L)
    }
    
    simpleD3Bar(
      data = dat,
      colors = c("#bd6916", "#166abd"),
      plotTitle = plot_title,
      sexVars = sex_vars
    )
  }
  
})


plot_d3_line <- reactive({
  if (isNational()) {
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

mapObj <- function() {
  if (input$aggr_level == "kom") {
    dk_sp$l2
  } else if (input$aggr_level == "region") {
    dk_sp$l1
  }
}

mapData <- function() {
  data_var <- prettyVariableSingular()
  keep_vars <- c("id", prettyAggr_level(), data_var)
  tmp <- copy(outputCasesD3Bar())
  # inx <- duplicated(tmp[, ..data_var])
  # # tmp[inx, (data_var) := jitter(get(data_var), amount = .1)]
  
  # MALES
  out_m <- mapObj()
  tmp_m <-
    tmp[get(ui_sex) == "male" &
          get(prettyAggr_level()) != "Unknown"]
  out_m@data <-
    merge(
      tmp_m,
      out_m@data,
      by.x = prettyAggr_level(),
      by.y = paste0("name_", lang),
      all.y = TRUE
    )
  out_m@data <- out_m@data[, ..keep_vars]
  setorder(out_m@data, id)
  
  # Female
  out_f <- mapObj()
  tmp_f <-
    tmp[get(ui_sex) == "female" &
          get(prettyAggr_level()) != "Unknown"]
  out_f@data <-
    merge(
      tmp_f,
      out_f@data,
      by.x = prettyAggr_level(),
      by.y = paste0("name_", lang),
      all.y = TRUE
    )
  out_f@data <- out_f@data[, ..keep_vars]
  setorder(out_f@data, id)
  
  # Combine for output
  list(male = out_m,
       female = out_f)
  
}

combinedMaps <- reactive({
  req((input$aggr_level == "region" || input$aggr_level == "kom"))
  name_lang <- paste0("name_", lang)
  fill_data <-
    rbind(mapData()$male@data, mapData()$female@data)[[prettyVariableSingular()]]
  
  if (input$count_rates == 2) {
    pal <- colorQuantile("YlOrRd", fill_data, n = 5, reverse = FALSE)
    if (selectPercentOrRate()) {
      labFormatter <- function(type, cuts) {
        n = length(cuts)
        cuts <- round(cuts, digits = 1)
        paste0(cuts[-n], "% &ndash; ", cuts[-1], "%")
      }
    } else {
      labFormatter <- function(type, cuts) {
        n = length(cuts)
        cuts <- round(cuts)
        paste0(cuts[-n], " &ndash; ", cuts[-1])
      }
    }
  } else {
    pal <- colorBin("YlOrRd",
                    fill_data,
                    bins = 5,
                    reverse = FALSE)
    labFormatter <- function(type, cuts) {
      n = length(cuts)
      cuts <- round(cuts)
      paste0(cuts[-n], " &ndash; ", cuts[-1])
    }
  }
  
  
  legend_title <- gsub("  ", "<br>", prettyVariableSingular())
  
  
  # Male map
  map_data <-  mapData()$male
  fill_colors <-
    ~ pal(map_data@data[[prettyVariableSingular()]])
  popup <- paste0(
    prettyAggr_level(),
    ": <strong>",
    map_data@data[[prettyAggr_level()]],
    "</strong><br><br>",
    map_data@data[[prettyVariableSingular()]]
  ) %>%
    lapply(htmltools::HTML)
  
  
  map_m <- makeLeaflet(
    map_data = map_data,
    fill_colors = fill_colors,
    label_popup = popup,
    mini_map_lines = dk_sp$mini_map_lines
  )
  
  # Female map
  map_data <-  mapData()$female
  fill_colors <-
    ~ pal(map_data@data[[prettyVariableSingular()]])
  popup <- paste0(
    prettyAggr_level(),
    ": <strong>",
    map_data@data[["name_dk"]],
    "</strong><br><br>",
    map_data@data[[prettyVariableSingular()]]
  ) %>%
    lapply(htmltools::HTML)
  
  
  map_f <- makeLeaflet(
    map_data = map_data,
    fill_colors = fill_colors,
    label_popup = popup,
    mini_map_lines = dk_sp$mini_map_lines
    
  ) %>%
    addLegend(
      "topright",
      pal = pal,
      values = fill_data,
      # colors =cols,
      title = legend_title,
      labels = legend_labels,
      layerId = "legend",
      labFormat = function(type, cuts, p = NULL) {
        type <- type
        cuts <- cuts
        
        labFormatter(type = type,
                     cuts = cuts)
      }
    )
  
  # Store maps on "map" reactiveValues object - these will be accessed by the
  # downloadHandler for downloading functionality. I do not know how to
  # download both maps together.
  map$map_f <- map_f
  
  # Need to add legend to male map in case it's downloaded without female map
  map$map_m <- map_m %>%
    addLegend(
      "topright",
      pal = pal,
      values = fill_data,
      # colors =cols,
      title = legend_title,
      labels = legend_labels,
      layerId = "legend",
      labFormat = function(type, cuts, p = NULL) {
        type <- type
        cuts <- cuts
        
        labFormatter(type = type,
                     cuts = cuts)
      }
    )
  combineWidgets(map_m, map_f, ncol = 2)
  
  
})




# DATATABLES --------------------------------------------------------------
dtCast <- reactive({
  # One dcast for both rates and counts
  
  dat <- outputCasesData()
  group_var <- prettyAggr_level()
  value_var <- prettyVariable()
  # x <- copy(dat)
  # setkeyv(dat, c(ui_sex, group_var))
  # if (!all.equal(x, dat, check.attributes = FALSE))
  
  subset_cols = c(group_var, value_var)
  out = cbind(dat["male", ..subset_cols], dat["female", ..value_var])
  setnames(out, c("group_var", paste0(value_var, rep(
    c("_male", "_female"), c(2, 2)
  ))))
  if (isNational() && is5YearMortality()) {
    return(out[group_var <= year_max - 4, ])
    
  } else {
    return(out)
  }
})


outputCountDTTable <- reactive({
  # Organizes data for DataTable outputs. Needs to be characters
  
  dat <- dtCast()
  # Subset to either counts or rates
  vars <-
    c("group_var",
      grep(prettyVariable()[1], colnames(dat), value = TRUE))
  
  dat <- dat[, ..vars]
  colnames(dat) <- c("group_var", "male", "female")
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
    col_convert <- c("male", "female", "Total")
    dat[, (col_convert) := lapply(.SD, as.numeric), .SDcols = col_convert]
    
  }
  
  setnames(
    dat,
    old = c("group_var", "male", "female"),
    new = c(prettyAggr_level(), rev(ui_sex_levels))
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
})

outputRateDTTable <- reactive({
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
  
  # If results are percentages - round with 1 sig fig. If results are rates -> 0
  # sig figs.
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

isNational <- reactive({
  input$aggr_level == "national"
})

is5YearMortality <- reactive({
  input$variable == "count_n_dead5"
})

# CHANGE UI BASED ON INPUTS -----------------------------------------------

output$varButtonChoices <- renderUI({
  # Gives a dynamic button UI. The buttons change depending on the selected
  # outcome Keep variables that have "count" in their name.
  #
  # When page loads, this UI element initally returns NULL to server fn(),
  # then it re-runs and returns the initial value - eg. "age". This means we
  # have to restrict the output that depends on this (which is nearly
  # everything) from running until a non-NULL value is supplied. This is
  # acheived by an if-statement in the validate() reactive.
  
  outcome_subset <- shiny_dat[[outcomeCode()]]$age
  var_names <-
    grep("count", names(outcome_subset), value = TRUE)
  var_names <- var_names[!grepl("mean", var_names)]
  
  # Select the plain language terms matching the variables in data
  variable_choices <-
    variable_ui[code_name %in% var_names, .(code_name, var_dk)]
  var_names <- variable_choices$code_name
  names(var_names) <- variable_choices$var_dk
  
  # If the previous selected var is available in the new outcome vars, make that
  # the default, else the first variable
  selected_var <- isolate(input$variable)
  if (is.null(selected_var) || !selected_var %in% var_names) {
    selected_var <- var_names[1]
  }
  selectInput(
    inputId = "variable",
    label = choose_var,
    choices = var_names,
    selectize = TRUE,
    selected = selected_var
    
  )
})


choiceYears <- reactive({
  # The following additional if-else logic is needed to stop the year count
  # always resetting to 2015 when changing aggr_level.
  
  input$aggr_level
  year_val <- isolate(input$year)
  if (year_val != "") {
    selected_year <- year_val
    # Set year-range to be used by udateSelectInput()
    if (isGeo() &&
        !is5YearMortality()) {
      year_range <- c(2009:year_max)
      if (year_val < 2009)
        selected_year <- 2009
      
    } else if (isGeo() &&
               is5YearMortality()) {
      year_range <- c(2009:(year_max - 4))
      if (year_val < 2009) {
        selected_year <- 2009
      } else if (year_val > (year_max - 4)) {
        selected_year <- year_max - 4
      }
      
    } else if (!isGeo() &&
               is5YearMortality()) {
      year_range <- c(2006:(year_max - 4))
      if (year_val > (year_max - 4)) {
        selected_year <- year_max - 4
      }
      
    } else {
      year_range <- c(2006:year_max)
      
    }
    return(list(selected_year = selected_year,
                year_range = year_range))
    
  } else {
    return(list(
      selected_year = year_max,
      year_range = 2006:year_max
    ))
    
  }
  
})


observe({
  req(input$variable)
  if (req(input$aggr_level) != "national") {
    # User can only select years >=2009 when viewing regional data and <=2012 when
    # viewing 5-year mortality
    updateSelectInput(
      session = session,
      inputId = "year",
      choices = choiceYears()$year_range,
      selected = choiceYears()$selected_year
    )
  }
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


# Switch tabs when isGeo == FALSE
observeEvent(input$aggr_level, {
  if (!isGeo())
    updateTabsetPanel(session = session,
                      inputId = "data_vis_tabs",
                      selected = ui_d3_figures)
})

# DOWNLOAD BUTTONS --------------------------------------------------------
output$downloadButton <- renderUI({
  if (!isNational()) {
    actionBttn(inputId = "download_bar",
               label = "Hente figure",
               size = "sm")
  } else if (isNational()) {
    actionBttn(inputId = "download_line",
               label = "Hente figure",
               size = "sm")
  }
  
})

map <- reactiveValues(dat = 0)
output$downloadMapsMale <- downloadHandler(
  filename = "map_male.png",
  content = function(file) {
    mapview::mapshot(map$map_m, file = file, cliprect = "viewport")
  }
)
output$downloadMapsFemale <- downloadHandler(
  filename = "map_female.png",
  content = function(file) {
    mapview::mapshot(map$map_f, file = file, cliprect = "viewport")
  }
)
# RENDER FUNCTIONS --------------------------------------------------------

# PLOT
#
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
output$table_counts <- renderDT({
  if (validate()) {
    outputCountDTTable()
  }
})

output$table_rates <- renderDT({
  if (validate()) {
    outputRateDTTable()
  }
})

output$maps <- renderCombineWidgets(combinedMaps())