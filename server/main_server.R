options(DT.options = list(
  pageLength = 20,
  dom = "Bt",
  buttons = c('copy', 'csv', 'pdf')
))

# callModule(profvis_server, "profiler")

# URL BOOKMARKING ---------------------------------------------------------


# TEXT RENDERING ----------------------------------------------------------

prettyOutcome <- reactive({
  
  outcomes_all[hjertetal_code == input$oCVD, name]
})

output$outcome_description <- renderUI({
  req(input$oCVD)
  
  keep_vars <- c("desc", "link")
  out_title <- tags$b(prettyOutcome())
  
  out <-
    outcome_descriptions[hjertetal_code == outcomeCode(), ..keep_vars]
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
    substr(outcomeCode(), 1, 1),
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
    
    text_var_desc(title_text = prettyVariable()[1],
                  lang = lang,
                  variable_ui = variable_ui,
                  selected_data_vars = selectedDataVars()[1],
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
    selected_data_vars = selectedDataVars(),
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
    selected_data_vars = selectedDataVars(),
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
  req(input$rate_count, input$varCVD, selectedDataVars())
  if (input$rate_count == 2) {
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
  if(validateIn()){
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

outcomeCode <- reactive({
  # Connect the input in normal language to the hjertetal_code. This is so we
  # can change the description without having to rename allll the datasets.
  input$oCVD
})

prettyAggr_level <- reactive({
  # Outputs same character string that's used in the UI input field
  aggr_choices[name_ht == input$agCVD, label]
  
})

prettyVariable <- reactive({
  # Outputs character string formatted for user.
  req(input$varCVD, input$agCVD)
  pretty_variable(
    lang = lang,
    data_var_name = selectedDataVars()[1],
    selected_rate_type = selectedRateType(),
    var_ui = variable_ui
  )
  
})

prettyVariableSingular <- reactive({
  prettyVariable()[as.integer(input$rate_count)]
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

selectPercentOrRate <- reactive({
  if (input$varCVD %in% c(
    "v9",
    "v13",
    "v14",
    "v15"
  )) {
    TRUE
  } else {
    FALSE
  }
  
})


# SUBSETTING ------------------------------------------------------
subsetOutcome <- reactive({
  # Cache subset based on outcome, aggr level, and theme
  shiny_dat[[outcomeCode()]][[input$agCVD]]
})

selectedRateType <- reactive({
  if (input$agCVD == "age") {
    "stratified"
  } else {
    "standardized"
  }
})


varCodeCVD <- reactive({
  input$varCVD
  variable_ui[shiny_code == input$varCVD, code_name]
})

selectedDataVars <- reactive({
  # Returns the column names to be used to subset the data - taking into account
  # raw or mean data
  
  var_stripped <- gsub("count_|rate_", "", varCodeCVD())
  grep_str <- paste0(var_stripped, "$")
  grep(grep_str, colnames(subsetOutcome()), value = TRUE)
})

subsetVars <- reactive({
  subset_vars(
    dat = subsetOutcome(),
    data_vars = selectedDataVars(),
    ag_lv = input$agCVD,
    select_raw_mean = selectRawOrMean(),
    pretty_variable = prettyVariable(),
    select_percent_rate = selectPercentOrRate(),
    pretty_ag_lv = prettyAggr_level()
  )
})

subsetYear <- reactive({
  # Subset the already partially subset data based on years
  subsetVars()[get(ui_year) == input$year,][, (ui_year) := NULL]
})


# FORMATTING :DATA FOR D3------------------------------------------------------
outputCasesData <- function() {
  
  # National level data shows all years
  if (!isNational()) {
    subsetYear()
  } else {
    subsetVars()
  }
}

dataD3Line <- reactive({
  # Replace value.var with reactive that corresponds to the variable the user selected
  data_d3_line(dat = dtCast(),
               pretty_var_singular = prettyVariableSingular(),
               ui_year = ui_year)
})

dataD3Bar <- reactive({
  # Restrict data to the user selected vairable, and give pretty column names
  data_d3_bar(
    pretty_var_singular = prettyVariableSingular(),
    ui_sex = ui_sex,
    pretty_aggr_level = prettyAggr_level(),
    count_rate = count_rate,
    subset_year = subsetYear()
  )
 
})


plot_d3_bar <- reactive({
  if (nrow(dataD3Bar()) > 0  &&
      !isNational()) {
    sex_vars <- ui_sex_levels
    color = c(graph_colors[1], graph_colors[2])
    plot_title = plotTitle()
    
    # For kommune data re-order based on rate or count
    dat <- copy(dataD3Bar())
    if (isKom()) {
      setorderv(dat, c(ui_sex, prettyVariableSingular()), order = -1L)
    }
    
    simpleD3Bar(
      data = dat,
      colors = c(graph_colors[1], graph_colors[2]),
      plotTitle = plot_title,
      sexVars = sex_vars,
      dataVar = prettyVariableSingular(),
      lang = lang
    )
  }
  
})


plot_d3_line <- reactive({
  if (isNational()) {
    
    sex_vars <- ui_sex_levels
    color = c(graph_colors[1], graph_colors[2])
    plot_title = plotTitle()
    simpleD3Line(
      data = dataD3Line(),
      colors = c(graph_colors[1], graph_colors[2]),
      plotTitle = plot_title,
      sexVars = sex_vars,
      dataVar = prettyVariableSingular(),
      lang = lang
    )
  }
})



# LEAFLET MAPS ------------------------------------------------------

mapObj <- reactive({
  map_obj(is_kom = isKom(),
          is_region = isRegion(),
          dk_sp = dk_sp)
})

mapData <- reactive({
  dat <-
    copy(outputCasesData()) # Make copy so not corrupt reactive data
  
  map_data(dat = dat,
           data_var = prettyVariable()[2],
           pretty_aggr_level = prettyAggr_level(),
           map_obj = mapObj(),
           ui_sex = ui_sex
           )
  
  
})

combinedMaps <- reactive({
  req((isRegion() || isKom()))
  maps_out <- maps_combine(var_name = prettyVariable()[2],
                           lang = lang,
                           subset_vars = subsetVars(),
                           ui_year = ui_year,
                           select_percent_rates = selectPercentOrRate(),
                           thousands_sep = thousands_sep,
                           dec_mark = dec_mark,
                           map_data_main = mapData(),
                           dk_sp = dk_sp,
                           pretty_aggr_level = prettyAggr_level())
    
  
  # Store maps on "map" reactiveValues object - these will be accessed by the
  # downloadHandler for downloading functionality. I do not know how to
  # download both maps together.
  map$map_f <- maps_out$map_f
  map$map_m <- maps_out$map_m
  # Need to add legend to male map in case it's downloaded without female map
  map$map_m_legend <- maps_out$map_m_legend
    
  map
  
})




# DATATABLES --------------------------------------------------------------
dtCast <- reactive({
  # One dcast for both rates and counts
  
  dt_cast(dat = outputCasesData(),
         group_var = prettyAggr_level(),
         value_var = prettyVariable(),
         is_national = isNational(),
         is_5year_mortality = is5YearMortality())
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
    percent_rate = selectPercentOrRate(),
    pretty_ag_lv = prettyAggr_level(),
    ui_sex_levels = ui_sex_levels,
    plot_title = plotTitle(), pretty_var_singular = prettyVariableSingular(),
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
  #
  nonZero_variable <- !is.null(input$varCVD)
  if (nonZero_variable) {
    length(selectedDataVars()) > 0 && input$oCVD != "" &&
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
  
  input$varCVD == "v15"
})


isPercentage <- reactive({
  any(
    input$varCVD == "v13",
    input$varCVD == "v14",
    input$varCVD == "v15",
    input$varCVD == "v9"
  )
  
})

# CHANGE UI BASED ON INPUTS -----------------------------------------------


# This requires an valid aggr_level input

validateSelectedVars <- reactive({

  req(input$agCVD)
  
  selected_var <- isolate(input$varCVD)
 
  
  validate_selected_vars(
    aggr_selected = input$agCVD,
    outcome_code = outcomeCode(),
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
  var_choice_out <- make_var_choices(selected_var = (validateSelectedVars()$selected_var),
                   var_names = (validateSelectedVars()$var_names),
                   valid_selection = (validateSelectedVars()$valid_selection),
                   aggr_selected_next = aggr_selected_next,
                   outcome_code = outcomeCode(),
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
    outcome_code = outcomeCode(),
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
output$downloadButton <- renderUI({
  req(input$agCVD)
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
  req(input$varCVD)
  if (validateIn() && !isNational() && !isKom()) {
    
    plot_d3_bar()
  }
})

output$d3_plot_line_html <- renderSimpleD3Line({
  req(input$varCVD)
  if (validateIn() && isNational()) {
    plot_d3_line()
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

# MAPS
output$map_male <- renderLeaflet({
  req(validateIn())
    combinedMaps()$map_m
})

output$map_female <- renderLeaflet({
  req(validateIn())
  combinedMaps()$map_f
})
