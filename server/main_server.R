options(DT.options = list(
  pageLength = 20,
  dom = "Bt",
  buttons = c('copy', 'csv', 'pdf')
))
# POPUP DISCLAIMER --------------------------------------------------------
showModal(
  modalDialog(
    title = "Beta testing Hjertetal2",
    easyClose = TRUE,
    fade = TRUE,
    tags$p(
      tags$b(
        "HjerteTal does not work with Internet Explorer. Please choose Chrome/Firefox/Safari/Edge"
      )
    ),
    tags$br(),
    tags$p(
      "This is a beta version of HjerteTal2 that should only be used for testing purposes.
    The data presented has not been evaluated for accuracy and should not be used for reporting."
    ),
    tags$p("Please send any feedback to mphelps@hjerteforeningen.dk"),
    tags$br(),
    tags$p("Click anywhere to dismiss")
  )
)

# callModule(profvis_server, "profiler")
# TEXT RENDERING ----------------------------------------------------------
output$outcome_description <- renderUI({
  req(input$outcome)
  
  out_title <- tags$b(input$outcome)
  out <-
    outcome_descriptions[hjertetal_code == outcomeCode(), .(desc_dk, link_dk)]
  # Add link for further reading - if link exists, otherwise just desc
  url <- a(ui_read_more,
           href = (out$link_dk),
           target = "_blank")
  
  if (out$link_dk != "na") {
    tagList(out_title, out$desc_dk, url)
  }
  else {
    tagList(out_title, out$desc_dk)
  }
})


# Some variables desc need to be different for diag, opr, and med outcomes.
# This statement switches between those cases.
replaceTypeString <- reactive({
  switch(
    substr(outcomeCode(), 1, 1),
    "b" = replace_type_string_opr,
    "d" = replace_type_string_diag,
    "m" = replace_type_string_med
  )
})

replaceOutcomeString <- reactive({
  replace_outcome_string <- input$outcome
  # Lowercase first character only (keeps abbreviations in caps)
  substr(replace_outcome_string, 1, 1) <-
    tolower(substr(replace_outcome_string, 1, 1))
  replace_outcome_string
})



replaceAggrLevelString <- reactive({
  aggr_choices[name_ht == input$aggr_level, tolower(name_dk_long)]
})



output$variable_desc <- renderUI({
  req(input$variable,
      input$year,
      input$outcome,
      selectedDataVars())
  
  isolate({
    # Append title to front of variable descr text
    
    title_text <- tolower(prettyVariable()[1])
    title_text <- tags$b(paste0("Den ", title_text))
    
    
    col_selection <- paste0("desc_general_", lang)
    desc_text <-
      variable_ui[code_name == selectedDataVars()[1], ..col_selection]
    
    # Replace sections of variable desc that are specific for
    # outcome/year/outcome-type
    
    desc_text <-
      gsub(
        "REPLACE_OUTCOME",
        replaceOutcomeString(),
        (desc_text$desc_general_dk),
        fixed = TRUE
      )
    # For some reason, some danish characters encoding is messed up after the
    # gsub fn(). This fixes that
    
    desc_text <- gsub("Ã¥", "å", desc_text, fixed = TRUE)
    desc_text <- desc_text <- gsub("alle hjerte-kar-sygdomme", replace_allCVD_string, desc_text)
    
    
    
    desc_text <-
      gsub("REPLACE_TYPE", replaceTypeString(), desc_text, useBytes = TRUE)
    
    desc_text <-
      gsub("REPLACE_YEAR", tolower(input$year), desc_text, useBytes = TRUE)
    
    tagList(title_text, (desc_text))
    
  })
})


rate_desc <- function() {
  # For rates
  title_text <- tolower(prettyVariable()[2])
  title_text <- tags$b(paste0("Den ", title_text))
  
  col_selection <-
    paste0("desc_", selectedRateType(), "_", lang)
  desc_text <-
    variable_ui[code_name == selectedDataVars()[1], ..col_selection]
  
  desc_text <-
    gsub("REPLACE_OUTCOME",
         replaceOutcomeString(),
         desc_text,
         fixed = TRUE)
  
  desc_text <-
    gsub("REPLACE_AGGR",
         replaceAggrLevelString(),
         desc_text,
         fixed = TRUE)
  
  # Fix Danish letters
  desc_text <- gsub("Ã¥", "å", desc_text, fixed = TRUE)
  
  desc_text <-
    gsub("REPLACE_TYPE", replaceTypeString(), desc_text, useBytes = TRUE)
  desc_text <-
    gsub("REPLACE_YEAR", tolower(input$year), desc_text, useBytes = TRUE)
  tagList(title_text, desc_text)
}


count_desc <- function() {
  # For counts
  title_text <-
    tags$b(paste0(ui_count_rate[1], " ", tolower(prettyVariable()[1])))
  
  col_selection <- paste0("desc_", "count", "_", lang)
  desc_text <-
    variable_ui[code_name == selectedDataVars()[1], ..col_selection]
  desc_text <-
    gsub("REPLACE_OUTCOME",
         replaceOutcomeString(),
         desc_text,
         fixed = TRUE)
  
  desc_text <-
    gsub("REPLACE_AGGR",
         replaceAggrLevelString(),
         desc_text,
         fixed = TRUE)
  desc_text <- gsub("Ã¥", "å", desc_text, fixed = TRUE)
  
  desc_text <-
    gsub("REPLACE_TYPE", replaceTypeString(), desc_text, useBytes = TRUE)
  desc_text <-
    gsub("REPLACE_YEAR", tolower(input$year), desc_text, useBytes = TRUE)
  
  tagList(title_text, desc_text)
}


output$rate_desc <- renderUI({
  rate_desc()
})

output$rate_desc_map <- renderUI({
  rate_desc()
})


output$count_desc <- renderUI({
  count_desc()
})


output$rate_count_desc <- renderUI({
  req(input$rate_count, input$variable, selectedDataVars())
  
  
  if (input$rate_count == 2) {
    rate_desc()
  } else {
    count_desc()
  }
  
})



plotTitle <- reactive({
  if (isNational()) {
    paste0(input$outcome, ": ", tolower(prettyVariable()[1]))
  } else {
    if (isGeo()) {
      paste0(
        input$outcome,
        ": ",
        tolower(prettyVariable()[1]),
        "  ",
        input$year,
        ", ",
        ui_moving_avg_desc
      )
    }
    else if (input$aggr_level == "edu") {
      paste0(
        input$outcome,
        ": ",
        tolower(prettyVariable()[1]),
        " (",
        ui_edu_age_range,
        ")  ",
        input$year,
        ", ",
        ui_moving_avg_desc
      )
    } else {
      paste0(input$outcome,
             ": ",
             tolower(prettyVariable()[1]),
             "  ",
             input$year)
    }
    
  }
})



# Titles
output$outcome_title <- renderText({
  input$outcome
})

output$outcome_title_dt <- renderText({
  req(input$data_vis_tabs == ui_data)
  plotTitle()
})

output$outcome_title_map <- renderText({
  req(input$data_vis_tabs == ui_map)
  plotTitle()
})


# Datatables titles
output$table1_title <- renderText({
  ui_count_rate[1]
})

output$table2_title <- renderText({
  prettyVariable()[2]
})


# Maps titles
output$map_title_male <- renderText(ui_sex_levels[2])
output$map_title_female <- renderText(ui_sex_levels[1])

# Tabs
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
  aggr_choices[name_ht == input$aggr_level, name_dk]
  
})

prettyVariable <- reactive({
  # Outputs character string formatted for user.
  req(input$variable, input$aggr_level)
  
  data_var_name <- selectedDataVars()[1]
  grep_selection <-
    paste0("var_rate_", selectedRateType(), "_", lang)
  col_names <- colnames(variable_ui)
  col_selection <- grep(grep_selection, col_names, value = TRUE)
  c(variable_ui[code_name == data_var_name, var_dk], paste0(variable_ui[code_name == data_var_name, ..col_selection]))
  
})

prettyVariableSingular <- reactive({
  prettyVariable()[as.integer(input$rate_count)]
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
  setnames(dat, c(ui_year, "male", "female")) # TODO: needs to be language agnostic
  
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
    color = c(graph_colors[1], graph_colors[2])
    plot_title = plotTitle()
    
    # For kommune data re-order based on rate or count
    dat <- copy(outputCasesD3Bar())
    if (isKom()) {
      setorderv(dat, c(ui_sex, prettyVariableSingular()), order = -1L)
    }
    
    simpleD3Bar(
      data = dat,
      colors = c(graph_colors[1], graph_colors[2]),
      plotTitle = plot_title,
      sexVars = sex_vars
    )
  }
  
})


plot_d3_line <- reactive({
  if (isNational()) {
    sex_vars <- ui_sex_levels
    color = c(graph_colors[1], graph_colors[2])
    plot_title = plotTitle()
    simpleD3Line(
      data = outputCasesD3Line(),
      colors = c(graph_colors[1], graph_colors[2]),
      plotTitle = plot_title,
      sexVars = sex_vars
    )
  }
})



# LEAFLET MAPS ------------------------------------------------------

mapObj <- function() {
  if (isKom()) {
    dk_sp$l2
  } else if (isRegion()) {
    dk_sp$l1
  }
}

mapData <- reactive({
  data_var <- prettyVariable()[2] # Only use rate data
  keep_vars <- c("id", prettyAggr_level(), data_var)
  tmp <-
    copy(outputCasesData()) # Make copy so not corrput reactive data
  
  # Set Zero values to NA - 0s mean <4 observations, so we don't know the actual
  # value
  tmp[get(data_var) == 0, (data_var) := NA]
  
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
  
  # Remove unneed vars and re-order data
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
  
  # Remove unneed vars and re-order data
  out_f@data <- out_f@data[, ..keep_vars]
  setorder(out_f@data, id)
  
  # Combine for output
  list(male = out_m,
       female = out_f)
  
})

combinedMaps <- reactive({
  req((isRegion() || isKom()))
  
  var_name <- prettyVariable()[2]
  name_lang <- paste0("name_", lang)
  
  fill_data <-
    subsetVars()[get(ui_year) >= 2009, ][[var_name]]
  
  fill_data[fill_data == 0] <- NA
  # Define breaks using the "pretty" algorithm
  map_breaks <-
    suppressWarnings(classIntervals(fill_data, style = "pretty", n = 5))
  pal <-
    colorBin(
      palette = "YlOrRd",
      bins = length(map_breaks$brks),
      domain = map_breaks$brks
    )
  
  if (selectPercentOrRate()) {
    # If variable is percent:
    labFormatter <- function(type, cuts) {
      n = length(cuts)
      cuts <- round(cuts, digits = 1)
      paste0(cuts[-n], "% &ndash; ", cuts[-1], "%")
    }
  } else {
    # If variable is rate:
    labFormatter <- function(type, cuts) {
      n = length(cuts)
      cuts_formatted <- formatC(
        round(cuts),
        digits = 0,
        format = "d",
        big.mark = thousands_sep,
        decimal.mark = dec_mark
      )
      paste0(cuts_formatted[-n], " &ndash; ", cuts_formatted[-1])
    }
  }
  
  
  legend_title <- gsub("  ", "<br>", var_name)
  popup_var_title_1 <- gsub("  .*", "", var_name)
  
  
  # Male map
  map_data <-  mapData()$male
  fill_colors <-
    ~ pal(map_data@data[[var_name]])
  
  
  popup <-
    makeMapPopup(geo_name = map_data@data[[prettyAggr_level()]],
                 var_title1 = popup_var_title_1,
                 map_data@data[[var_name]])
  
  map_m <- makeLeaflet(
    map_data = map_data,
    fill_colors = fill_colors,
    label_popup = popup,
    mini_map_lines = dk_sp$mini_map_lines,
    element_id = "map_male"
  )
  
  # Female map
  map_data <-  mapData()$female
  
  fill_colors <-
    ~ pal(map_data@data[[var_name]])
  popup <-
    makeMapPopup(geo_name = map_data@data[[prettyAggr_level()]],
                 var_title1 = popup_var_title_1,
                 map_data@data[[var_name]])
  
  
  map_f <- makeLeaflet(
    map_data = map_data,
    fill_colors = fill_colors,
    label_popup = popup,
    mini_map_lines = dk_sp$mini_map_lines,
    element_id = "map_female"
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
  map$map_m <- map_m
  # Need to add legend to male map in case it's downloaded without female map
  map$map_m_legend <- map_m %>%
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
  map
})




# DATATABLES --------------------------------------------------------------
dtCast <- reactive({
  # One dcast for both rates and counts
  
  dat <- outputCasesData()
  group_var <- prettyAggr_level()
  value_var <- prettyVariable()
  
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
  if (input$aggr_level != "national") {
    # Only calculate bottom margins for "age" - other aggr levels don't include
    # full data
    totals <-
      dat[, colSums(dat[, .(male, female, Total)], na.rm = TRUE)]
    
    # Convert entire table to character to we can rbind() totals
    dat <- dat[, lapply(.SD, as.character)]
    # Rbind totals
    dat <- rbindlist(list(dat, as.list(c("Total", totals))), use.names = FALSE)
    
    # Convert back to numeric
    col_convert <- c("male", "female", "Total")
    dat[, (col_convert) := lapply(.SD, as.numeric), .SDcols = col_convert]
    
    # Format numbers according to language
    dat[, (col_convert) := lapply(.SD, function(x) {
      formatC(
        x,
        digits = 0,
        format = "d",
        big.mark = thousands_sep,
        decimal.mark = dec_mark
      )
    }), .SDcols = col_convert]
  }
  
  setnames(
    dat,
    old = c("group_var", "male", "female"),
    new = c(prettyAggr_level(), rev(ui_sex_levels))
  )
  if (isKom()) {
    makeCountKomDT(
      dat,
      group_var = prettyAggr_level(),
      thousands_sep = thousands_sep,
      dt_title = plotTitle(),
      messageBottom = paste0(ui_count_rate[1], " ", tolower(prettyVariable()[1]))
    )
  } else {
    makeCountDT(
      dat,
      group_var = prettyAggr_level(),
      thousands_sep = thousands_sep,
      dt_title = plotTitle(),
      messageBottom = paste0(ui_count_rate[1], " ", tolower(prettyVariable()[1]))
    )
  }
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
  
  col_convert <- c("male", "female")
  dat[, (col_convert) := lapply(.SD, function(x) {
    formatC(
      x,
      digits = digits,
      format = "f",
      big.mark = thousands_sep,
      decimal.mark = dec_mark
    )
  }), .SDcols = col_convert]
  
  
  setnames(
    dat,
    old = c("group_var", "male", "female"),
    new = c(prettyAggr_level(), ui_sex_levels)
  )
  # colnames(dat) <- c(group_var, ui_sex_levels)
  if (isKom()) {
    makeRateKomDT(
      dat = dat,
      group_var = prettyAggr_level(),
      dt_title = plotTitle(),
      messageBottom = prettyVariableSingular()
      )
    
  } else {
    makeRateDT(dat = dat,
               group_var = prettyAggr_level(),
               dt_title = plotTitle(),
               messageBottom = prettyVariableSingular()
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
  if (isKom()) {
    input$year >= 2009
  } else if (input$aggr_level != "kom") {
    TRUE
  } else {
    FALSE
  }
})


isKom <- reactive({
  input$aggr_level == "kom"
})

isRegion <- reactive({
  input$aggr_level == "region"
})

isGeo <- reactive({
  isKom() ||
    isRegion()
})


isNational <- reactive({
  input$aggr_level == "national"
})

is5YearMortality <- reactive({
  input$variable == "count_n_dead5"
})

# CHANGE UI BASED ON INPUTS -----------------------------------------------


# This requires an valid aggr_level input

validateSelectedVars <- reactive({
  # Evalutes current variable selection to see if it is a valid selection for
  # the current selection of outcome and aggr_level combination. Returns
  # binary valid T/F, as well as current selection and current valid var
  # possibilities.
  req(input$aggr_level)
  
  aggr_selected <- input$aggr_level
  
  
  outcome_subset <- shiny_dat[[outcomeCode()]][[aggr_selected]]
  
  
  var_names <- valid_output_combos[outcome == outcomeCode() &
                                     aggr_level == aggr_selected, unique(var)]
  
  
  # Remove columns with data that should not be shown to user
  var_names <- var_names[!var_names %in% variables_not_used]
  
  # Select the plain language terms matching the variables in data
  variable_choices <-
    variable_ui[code_name %in% var_names, .(code_name, var_dk)]
  var_names <- variable_choices$code_name
  names(var_names) <- variable_choices$var_dk
  
  
  selected_var <- isolate(input$variable)
  
  # On start, var_selected is NULL, so set default value of validate_selection
  # to TRUE, so tables are shown
  validate_selection <- TRUE
  
  logic <- !(selected_var %in% var_names)
  if (length(logic) > 0 && logic == TRUE) {
    validate_selection <- FALSE
  }
  
  list(
    selected_var = selected_var,
    var_names = var_names,
    valid_selection = validate_selection
  )
})

output$varChoices <- renderUI({
  req(input$aggr_level)
  # Gives a dynamic button UI. The buttons change depending on the selected
  # outcome Keep variables that have "count" in their name.
  #
  # This next code allows the variable chosen by the user to remain, when
  # switching to a new outcome, while on a aggr_level not supported with the new
  # outcome. F.x. Switch from all-CVD, 30-day mortality, kommune-level, to
  # hjerteklapoperation. Hjerteklaoperation only supports 30-day mort at
  # national level, so the variable is switched to incidence.
  #
  # If the previous selected var is not available, test to see if it is
  # available in the previously selected aggr_level. If not to both, set
  # selected_var to be the first variable.
  selected_var <- validateSelectedVars()$selected_var
  var_names <- validateSelectedVars()$var_names
  valid_selection <- validateSelectedVars()$valid_selection
  if (is.null(selected_var)) {
    # If no previous selection:
    selected_var <- var_names[1]
  } else if (!valid_selection) {
    # If selected var not in current selection AND...
    aggr_selected_next <-
      isolate(aggrButtonChoices()$selected_aggr)
    if (is.null(aggr_selected_next)) {
      aggr_selected_next <- "national"
    }
    # Variable available for aggr_selected_next and outcome
    var_names_2 <-
      valid_output_combos[outcome == outcomeCode() &
                            aggr_level == aggr_selected_next,
                          unique(var)]
    if (!selected_var %in% var_names_2) {
      # ...selected var also not in set of vars attached to previously
      # selected aggr_level: Set var to incidence
      selected_var <- var_names[1]
    } else {
      var_names <- var_names_2
    }
  }
  
  selectInput(
    inputId = "variable",
    label = choose_var,
    choices = var_names,
    selectize = TRUE,
    selected = selected_var
  )
})

aggrButtonChoices <- reactive({
  # Dynamically chanages which aggre_level options are available depending on
  # which outcome and which variable is selected
  
  var_selected <- input$variable
  valid_output_combos <-
    valid_output_combos[outcome == outcomeCode()]
  
  # When app first starts, input$variable will be null, but need to get range of
  if (is.null(input$variable))
    var_selected <- valid_output_combos[1, var]
  
  
  aggr_level_choices <-
    valid_output_combos[outcome == outcomeCode() &
                          var == var_selected, unique(aggr_level)]
  
  # When switching between d, b, and m outcomes, this will return NULL at first calling
  if (length(aggr_level_choices) == 0)
    return(NULL)
  
  aggr_choices <- aggr_choices[name_ht %in% aggr_level_choices]
  row.names(aggr_choices) <- aggr_choices$name_dk
  button_vals <-
    setNames(split(aggr_choices$name_ht, seq(nrow(aggr_choices))),
             row.names(aggr_choices))
  
  
  # If the previous selected aggr_level is available in the new outcome vars, make that
  # the default, else the first variable
  selected_aggr <- isolate(input$aggr_level)
  if (is.null(selected_aggr) ||
      !selected_aggr %in% aggr_choices$name_ht) {
    selected_aggr <- aggr_choices$name_ht[1]
  }
  
  html_output <- radioGroupButtons(
    inputId = "aggr_level",
    label = choose_aggr_lv,
    choices = button_vals,
    justified = TRUE,
    direction = "vertical",
    selected = selected_aggr
  )
  
  return(
    list(
      button_vals = button_vals,
      selected_aggr = selected_aggr,
      html_output = html_output
    )
  )
})


output$aggrButtonChoices <- renderUI({
  aggrButtonChoices()$html_output
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
  # Disable "rate/count" when when not showing figure tab
  shinyjs::toggleState(id = "rate_count",
                       condition = input$data_vis_tabs == ui_d3_figures)
  
})


observe({
  # Shows map tab only when geo data is selected
  shinyjs::toggle(
    condition = (isKom() ||
                   isRegion()),
    selector = paste0("#data_vis_tabs li a[data-value=", ui_map, "]")
  )
})


observe({
  # Hides Figures tab when showing kommne level
  shinyjs::toggle(
    condition = (input$aggr_level != "kom"),
    selector = paste0("#data_vis_tabs li a[data-value=", ui_d3_figures, "]")
  )
})



# Switch tabs when isGeo == FALSE
observeEvent(input$aggr_level, {
  if (input$data_vis_tabs == ui_map && !isGeo())
    updateTabsetPanel(session = session,
                      inputId = "data_vis_tabs",
                      selected = ui_d3_figures)
})

# Switch tabs when landing on kommune Figures tab (since this tab should not be
# shown to users)
observeEvent(input$aggr_level, {
  if (isKom() && input$data_vis_tabs == ui_d3_figures)
    updateTabsetPanel(session = session,
                      inputId = "data_vis_tabs",
                      selected = ui_map)
})


# DOWNLOAD BUTTONS --------------------------------------------------------
output$downloadButton <- renderUI({
  req(input$aggr_level)
  if (!isNational()) {
    actionBttn(inputId = "download_bar",
               label = ui_download_graph,
               size = "sm")
  } else if (isNational()) {
    actionBttn(inputId = "download_line",
               label = ui_download_graph,
               size = "sm")
  }
  
})

map <- reactiveValues(dat = 0)
output$downloadMapsMale <- downloadHandler(
  filename = "map_male.png",
  content = function(file) {
    mapshot(
      map$map_m_legend,
      file = file,
      selector = "#map_male",
      vwidth = 483,
      vheight = 590
    )
  }
)
output$downloadMapsFemale <- downloadHandler(
  filename = "map_female.png",
  content = function(file) {
    mapshot(
      map$map_f,
      file = file,
      selector = "#map_female",
      vwidth = 483,
      vheight = 590
    )
  }
)
# RENDER FUNCTIONS --------------------------------------------------------

# PLOT
#
output$d3_plot_bar <- renderSimpleD3Bar({
  req(input$aggr_level, input$variable)
  if (validate() && !isNational() && !isKom()) {
    plot_d3_bar()
  }
})

output$d3_plot_line_html <- renderSimpleD3Line({
  if (validate() && isNational()) {
    plot_d3_line()
  }
  
})

# DATATABLES
output$table_counts <- renderDT({
  req(validateSelectedVars()$valid_selection)
  if (validate()) {
    outputCountDTTable()
  }
})

output$table_rates <- renderDT({
  req(validateSelectedVars()$valid_selection)
  if (validate()) {
    outputRateDTTable()
  }
})

# MAPS
output$map_male <- renderLeaflet({
  req(validateSelectedVars()$valid_selection)
  combinedMaps()$map_m
})

output$map_female <- renderLeaflet({
  req(validateSelectedVars()$valid_selection)
  combinedMaps()$map_f
})
