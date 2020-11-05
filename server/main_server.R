options(DT.options = list(
  pageLength = 20,
  dom = "Bt",
  buttons = c('copy', 'csv', 'pdf')
))

# callModule(profvis_server, "profiler")
# TEXT RENDERING ----------------------------------------------------------

prettyOutcome <- reactive({
  
  outcomes_all[hjertetal_code == input$oCVD, name]
})

output$outcome_description <- renderUI({
  req(input$oCVD)
  
  keep_vars <- c("desc", "link")
  out_title <- tags$b(prettyOutcome())
  
  out <-
    outcome_descriptions[hjertetal_code == input$oCVD, ..keep_vars]
  # Add link for further reading - if link exists, otherwise just desc
  url <- a(ui_read_more,
           href = (out$link),
           target = "_blank")
  
  if (out$link != "na") {
    tagList(out_title, out$desc, url)
  }
  else {
    tagList(out_title, out$desc)
  }
})


# Some variables desc need to be different for diag, opr, and med outcomes.
# This statement switches between those cases.
replaceTypeString <- reactive({
  switch(
    substr(input$oCVD, 1, 1),
    "b" = replace_type_string_opr,
    "d" = replace_type_string_diag,
    "m" = replace_type_string_med
  )
})

replaceOutcomeString <- reactive({
  replace_outcome_string <- prettyOutcome()
  # Lowercase first character only (keeps abbreviations in caps)
  substr(replace_outcome_string, 1, 1) <-
    tolower(substr(replace_outcome_string, 1, 1))
  replace_outcome_string
  
})

replaceAggrLevelString <- reactive({
  aggr_choices[name_ht == input$agCVD, tolower(label_long)]
})

output$variable_desc <- renderUI({
  req(input$varCVD)
  
  isolate({
    # Append title to front of variable descr text
    
    text_var_desc(
      title_text = prettyVariable()[1],
      lang = lang,
      variable_ui = variable_ui,
      var_shiny_code = input$varCVD,
      replace_outcome_string = replaceOutcomeString(),
      replace_type_string = replaceTypeString(),
      replace_allCVD_string = replace_allCVD_string
    )
  })
})

textCountDesc <- reactive({
  text_count_desc(
    pretty_var = prettyVariable(),
    ui_count_rate = ui_count_rate,
    lang = lang,
    var_shiny_code = input$varCVD,
    replace_outcome_string = replaceOutcomeString(),
    replace_agg_level_string = replaceAggrLevelString(),
    replace_type_string = replaceTypeString(),
    input_year = input$year
  )
})

texRateDesc <- reactive({
  text_rate_desc(
    title_text = prettyVariable()[2],
    lang = lang,
    selected_rate_type = selectedRateType(),
    var_shiny_code = input$varCVD,
    replace_outcome_string = replaceOutcomeString(),
    replace_agg_level_string = replaceAggrLevelString(),
    replace_type_string = replaceTypeString(),
    input_year = input$year,
    is_national = isNational()
  )
})

output$rate_desc <- renderUI({
  texRateDesc()
})

output$rate_desc_map <- renderUI({
  texRateDesc()
})


output$count_desc <- renderUI({
  textCountDesc()
})


output$rate_count_desc <- renderUI({
  req(input$rate_count, input$varCVD, validateIn())
  if (input$rate_count == "rate") {
    texRateDesc()
  } else {
    textCountDesc()
  }
  
})



plotTitle <- reactive({
  if (isNational()) {
    paste0(prettyOutcome(), ": ", tolower(prettyVariable()[1]))
  } else {
    if (isGeo()) {
      paste0(
        prettyOutcome(),
        ": ",
        tolower(prettyVariable()[1]),
        "  ",
        input$year,
        ", ",
        ui_moving_avg_desc
      )
    }
    else if (input$agCVD == "edu") {
      paste0(
        prettyOutcome(),
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
      paste0(prettyOutcome(),
             ": ",
             tolower(prettyVariable()[1]),
             "  ",
             input$year)
    }
    
  }
})



# Titles
output$outcome_title <- renderText({
  prettyOutcome()
})

output$outcome_title_dt <- renderText({
  req(input$data_vis_tabs == ui_data)
  if (validateIn()) {
    plotTitle()
  }
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

prettyAggr_level <- reactive({
  # Outputs same character string that's used in the UI input field
  aggr_choices[name_ht == input$agCVD, label]
  
})

prettyVariable <- reactive({
  # Outputs character string formatted for user.
  req(input$varCVD, input$agCVD)
  
  pretty_variable(
    lang = lang,
    var_shiny_code = input$varCVD,
    selected_rate_type = selectedRateType(),
    var_ui = variable_ui
  )
  
})

prettyVariableSingular <- reactive({
  input$rate_count
  x <- 1
  if (input$rate_count == "rate")
    x <- 2
  prettyVariable()[x]
})


# SELECTORS ---------------------------------------------------------------
selectRawOrMean <- reactive({
  # Returns TRUE if raw count data should be used. FALSE if moving avg data
  # should be used
  if (input$agCVD %in% c("age", "national")) {
    TRUE
  } else if (input$agCVD %in% c("edu", "region", "kom", "ethnicity")) {
    FALSE
  }
})

# isPercentage <- reactive({
#   any(
#     input$varCVD == "v13",
#     input$varCVD == "v14",
#     input$varCVD == "v15",
#     input$varCVD == "v9"
#   )
#   
# })


isPercentage <- reactive({
  if (input$varCVD %in% c("v9",
                          "v11",
                          "v12",
                          "v13",
                          "v16",
                          "v17",
                          "v18")) {
    TRUE
  } else {
    FALSE
  }
  
})

numDigitsCVD <- reactive({
  # Only have decimal when showing percentages, otherwise round to whole units
  digits = 0
  if (input$rate_count == 2 & isPercentage()) {
    digits = 1
  }
  digits
})


# SUBSETTING ------------------------------------------------------
subsetOutcome <- reactive({
  # Cache subset based on outcome, aggr level, and theme

  shiny_dat[[input$oCVD]]
  })

subsetAgg <- reactive({
  subsetOutcome()[[input$agCVD]]
})

subsetVar <- reactive({
  subsetAgg()[[input$varCVD]]
})

subsetData <- reactive({
  x <-
    subsetVar()[, .(year, sex, grouping, count, rate)]
  
  if(isPercentage()) {
    x[, rate := rate / 1e3]
  }
  
  setnames(
    x,
    old = c("year", "sex", "grouping", "count", "rate"),
    new = c(
      ui_year,
      ui_sex,
      prettyAggr_level(),
      prettyVariable()
      
    )
  )
  
  x
})

subsetDataYear <- reactive({
  subsetData()[get(ui_year) == input$year]
})

selectedRateType <- reactive({
  if (input$agCVD == "age") {
    "stratified"
  } else {
    "standardized"
  }
})



# PLOTLY------------------------------------------------------
outputCasesData <- function() {
  # National level data shows all years
  if (!isNational()) {
    subsetYear()
  } else {
    subsetVars()
  }
}


plotly_bar_cvd <- reactive({
  make_plotly_bar_cvd(
    x = copy(subsetDataYear()),
    num_digits = numDigitsCVD(),
    pretty_aggr_level = prettyAggr_level(),
    pretty_variable = prettyVariableSingular(),
    sex = ui_sex,
    sex_levels = rev(ui_sex_levels)
    
  ) %>%
    plotly_config(
      plot_title = html_wrap(plotTitle(), width = 60),
      axis_font_size = axis_font_size,
      tick_font_size = tick_font_size,
      legend_font_size = legend_font_size,
      axis_title_x = prettyAggr_level(),
      axis_title_y = html_wrap(prettyVariableSingular(), width = 27),
      dec_mark = dec_mark,
      thousands_sep = thousands_sep,
      legend_order = "normal",
      num_digits = numDigitsCVD(),
      file_suffix = plotTitle()
    )
})


plotly_line_cvd <- reactive({
  
  make_plotly_cvd(
    x = copy(subsetData()),
    num_digits = numDigitsCVD(),
    pretty_variable = prettyVariableSingular(),
    sex = ui_sex,
    sex_levels = rev(ui_sex_levels),
    count_rate = input$rate_count
  ) %>%
    plotly_config(
      plot_title = html_wrap(plotTitle(), width = 60),
      axis_font_size = axis_font_size,
      tick_font_size = tick_font_size,
      legend_font_size = legend_font_size,
      axis_title_x = ui_year,
      axis_title_y = html_wrap(prettyVariableSingular(), width = 27),
      dec_mark = dec_mark,
      thousands_sep = thousands_sep,
      legend_order = "normal",
      num_digits = numDigitsCVD(),
      file_suffix = plotTitle()
    )
})


# LEAFLET MAPS ------------------------------------------------------

mapObj <- reactive({
  map_obj(is_kom = isKom(),
          is_region = isRegion(),
          dk_sp = dk_sf)
})


mapData <- reactive({
  
  dat <-
    copy(subsetDataYear()) # Make copy so not corrupt reactive data
  
  map_data(
    dat = dat,
    data_var = prettyVariable()[2],
    pretty_aggr_level = prettyAggr_level(),
    map_obj = mapObj(),
    ui_sex = ui_sex
  )
  
  
})

combinedMaps <- reactive({
  req((isRegion() || isKom()))
  
  maps_out <- maps_combine(
    var_name = prettyVariable()[2],
    lang = lang,
    subset_vars = subsetData(),
    ui_year = ui_year,
    select_percent_rates = isPercentage(),
    thousands_sep = thousands_sep,
    dec_mark = dec_mark,
    map_data_main = mapData(),
    dk_sp = dk_sf,
    pretty_aggr_level = prettyAggr_level()
  )
  
  return(maps_out)
})




# DATATABLES --------------------------------------------------------------
dtCast <- reactive({
  # One dcast for both rates and counts
  x <- subsetDataYear()
  if(isNational())
    x <- subsetData()
  dt_cast(
    dat = x,
    group_var = prettyAggr_level(),
    value_var = prettyVariable(),
    is_national = isNational(),
    is_5year_mortality = is5YearMortality()
  )
})


outputCountDTTable <- reactive({
  # Organizes data for DataTable outputs. Needs to be characters
  
  DTtables_count(
    dat = dtCast(),
    ag_lv = input$agCVD,
    pretty_ag_lv = prettyAggr_level(),
    pretty_vars = prettyVariable(),
    sex_levels = ui_sex_levels,
    ui_count_rate = ui_count_rate,
    thousands_sep = thousands_sep,
    dec_mark = dec_mark,
    plot_title = plotTitle(),
    is_kom = isKom()
  )
})

outputRateDTTable <- reactive({
  DTtables_rate(
    dat = dtCast(),
    pretty_vars = prettyVariable(),
    percent_rate = isPercentage(),
    pretty_ag_lv = prettyAggr_level(),
    ui_sex_levels = ui_sex_levels,
    plot_title = plotTitle(),
    pretty_var_singular = prettyVariableSingular(),
    is_kom = isKom(),
    thousands_sep = thousands_sep,
    dec_mark = dec_mark
  )
  
  
})

# VALIDATE BEFORE PLOTING -------------------------------------------------
validateIn <- reactive(label = "validate", {
  # Returns TRUE if passes and FALSE if any condition fails. This is needed to
  # stop the plots and tables trying to render when they have inproper input.
  # I.e. when switching between outcomes, the variable inupt is -
  nonZero_variable <- !is.null(input$varCVD) && !is.null(subsetVar())
  if (nonZero_variable) {
    input$oCVD != "" &&
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
  } else if (input$agCVD != "kom") {
    TRUE
  } else {
    FALSE
  }
})


isKom <- reactive({
  input$agCVD == "kom"
})

isRegion <- reactive({
  input$agCVD == "region"
})

isGeo <- reactive({
  isKom() ||
    isRegion()
})


isNational <- reactive({
  input$agCVD == "national"
})

is5YearMortality <- reactive({
  any(input$varCVD == "v13",
      input$varCVD == "v18")
})



# CHANGE UI BASED ON INPUTS -----------------------------------------------


# This requires an valid aggr_level input

validateSelectedVars <- reactive({
  req(input$agCVD)
  
  selected_var <- isolate(input$varCVD)
  
  validate_selected_vars(
    aggr_selected = input$agCVD,
    outcome_code = input$oCVD,
    variables_not_used = variables_not_used,
    lang = lang,
    selected_var = selected_var
  )
  
})

output$varChoices <- renderUI({
  req(input$agCVD)
  
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
  aggr_selected_next <-
    isolate(aggrButtonChoices()$selected_aggr)
  var_choice_out <-
    make_var_choices(
      selected_var = (validateSelectedVars()$selected_var),
      var_names = (validateSelectedVars()$var_names),
      valid_selection = (validateSelectedVars()$valid_selection),
      aggr_selected_next = aggr_selected_next,
      outcome_code = input$oCVD,
      valid_output_combos = valid_output_combos
    )
  
  selectInput(
    inputId = "varCVD",
    label = choose_var,
    choices = var_choice_out$var_names,
    selectize = TRUE,
    selected = var_choice_out$selected_var
  )
})

aggrButtonChoices <- reactive({
  # Dynamically chanages which aggre_level options are available depending on
  # which outcome and which variable is selected
  input_aggr_level <- isolate(input$agCVD)
  
  ag_choice_out <- make_agg_choices(
    var_selected = input$varCVD,
    outcome_code = input$oCVD,
    valid_output_combos = valid_output_combos,
    aggr_choices = aggr_choices,
    input_aggr_level = input_aggr_level
  )
  
  if (is.null(ag_choice_out))
    return(NULL)
  
  html_output <- radioGroupButtons(
    inputId = "agCVD",
    label = choose_aggr_lv,
    choices = ag_choice_out$button_vals,
    justified = TRUE,
    direction = "vertical",
    selected = ag_choice_out$selected_aggr
  )
  
  return(
    list(
      button_vals = ag_choice_out$button_vals,
      selected_aggr = ag_choice_out$selected_aggr,
      html_output = html_output
    )
  )
})


output$aggrButtonChoices <- renderUI({
  aggrButtonChoices()$html_output
})

choiceYears <- reactive({
  # The following additional if-else logic is needed to stop the year count
  # always resetting to year_max when changing aggr_level.
  input$agCVD
  year_val <- isolate(input$year)
  
  make_year_choices(
    year_val = year_val,
    is_geo = isGeo(),
    is_5year_mortality = is5YearMortality()
  )
})


observe(label = "updateYear", {
  req(input$varCVD)
  if (req(input$agCVD) != "national") {
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

observe(label = "disableYear", {
  # Disable "year" when showing longitudinal data
  shinyjs::toggleState(id = "year",
                       condition = input$agCVD != "national")
  
})


observe(label = "diableRate", {
  # Disable "rate/count" when when not showing figure tab
  shinyjs::toggleState(id = "rate_count",
                       condition = input$data_vis_tabs == ui_d3_figures)
  
})


observe(label = "tabsMaps", {
  # Shows map tab only when geo data is selected
  shinyjs::toggle(
    condition = (isKom() ||
                   isRegion()),
    selector = paste0("#data_vis_tabs li a[data-value=", ui_map, "]")
  )
})


observe(label = "hideGraph", {
  # Hides Figures tab when showing kommne level
  shinyjs::toggle(
    condition = (input$agCVD != "kom"),
    selector = paste0("#data_vis_tabs li a[data-value=", ui_d3_figures, "]")
  )
})



# Switch tabs when isGeo == FALSE
observeEvent(label = "forceTabSwitchMap", input$agCVD, {
  if (input$data_vis_tabs == ui_map && !isGeo())
    updateTabsetPanel(session = session,
                      inputId = "data_vis_tabs",
                      selected = ui_d3_figures)
})

# Switch tabs when landing on kommune Figures tab (since this tab should not be
# shown to users)
observeEvent(label = "forceTabSwicthKom", input$agCVD, {
  if (isKom() && input$data_vis_tabs == ui_d3_figures)
    updateTabsetPanel(session = session,
                      inputId = "data_vis_tabs",
                      selected = ui_map)
})


# DOWNLOAD BUTTONS --------------------------------------------------------
output$downloadMapsMale <- downloadHandler(
  filename = "map_male.png",
  content = function(file) {
    make_static_map(
      dat = combinedMaps()$fill_data_male,
      map_obj = mapData()$male,
      mini_map_lines = dk_sf$mini_map_lines,
      pretty_variable = prettyVariable()[2],
      plot_title = plotTitle(),
      sex = ui_sex_levels[2],
      thousands_sep = thousands_sep,
      dec_mark = dec_mark
    ) %>% ggsave(
      filename = file,
      plot = .,
      width = 12,
      height = 20,
      units = "cm",
      scale = 1.5
    )
  }
)
output$downloadMapsFemale <- downloadHandler(
  filename = "map_female.png",
  content = function(file) {
    make_static_map(
      dat = combinedMaps()$fill_data_female,
      map_obj = mapData()$female,
      mini_map_lines = dk_sf$mini_map_lines,
      pretty_variable = prettyVariable()[2],
      plot_title = plotTitle(),
      sex = ui_sex_levels[1],
      thousands_sep = thousands_sep,
      dec_mark = dec_mark
    ) %>% ggsave(
      filename = file,
      plot = .,
      width = 12,
      height = 20,
      units = "cm",
      scale = 1.5
    )
  }
)
# RENDER FUNCTIONS --------------------------------------------------------

# PLOT
#
output$d3_plot_bar <- renderPlotly({
  req(input$varCVD)
  if (validateIn() && !isNational() && !isKom()) {
    plotly_bar_cvd()
  }
})

output$plotly_line_cvd <- renderPlotly({
  req(input$varCVD)
  if (validateIn() && isNational()) {
    
    plotly_line_cvd()
  }
  
})

# DATATABLES
output$table_counts <- renderDT({
  req(validateIn())
  outputCountDTTable()
})

output$table_rates <- renderDT({
  req(validateIn())
  outputRateDTTable()
  
})

output$map_male <- renderLeaflet({
  req(validateIn())
  combinedMaps()$map_m
})

output$map_female <- renderLeaflet({
  req(validateIn(), subsetData())
  combinedMaps()$map_f
})
