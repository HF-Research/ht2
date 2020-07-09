# TEXT --------------------------------------------------------------------
prettyOutcomeChd <- reactive({
  outcome_descriptions_chd[ht.code == input$oCHD, name]
})
output$outcome_description_chd <- renderUI({
  req(input$oCHD)
  outcome_desc_chd(
    pretty_ouctome_chd = prettyOutcomeChd(),
    outcome_description_chd = outcome_description_chd,
    oCHD = input$oCHD,
    ui_read_more = ui_read_more
  )
  
})

prettyVarChdUnits <- reactive({
  x <- prettyVarChd()
  x[c("name_count", "name_rate")][as.integer(input$rate_count_chd)]
})

prettyVarChd <- reactive({
  unlist(var_descriptions_chd[code_name == input$var_chd, .(name, name_count, name_rate)])
  
})



replaceOutcomeStringChd <- reactive({
  replace_outcome_string <- prettyOutcomeChd()
  # Lowercase first character only (keeps abbreviations in caps)
  substr(replace_outcome_string, 1, 1) <-
    tolower(substr(replace_outcome_string, 1, 1))
  replace_outcome_string
})


output$variable_desc_chd <- renderUI({
  
  req(input$var_chd, input$oCHD,
      selectedDataVarsChd())
  
  isolate({
    # Append title to front of variable descr text
    variable_desc_chd(
      title_text = prettyVarChd()[1],
      var_descriptions_chd = var_descriptions_chd,
      var_chd = input$var_chd,
      replace_outcome_string_chd = replaceOutcomeStringChd(),
      ui_replace_all_chd = ui_replace_all_chd
      )
  })
})

dataTitle <-
  reactive({
    paste0(prettyOutcomeChd(), ": ", tolower(prettyVarChd()[1]))
  })

# DATA MUNGING --------------------------------------------------------------
subsetOutcomeChd <- reactive({
  # Cache subset based on outcome
  shiny_dat_chd[[input$agCHD]][[input$oCHD]]
})


selectedDataVarsChd <- reactive({
  # Returns the column names to be used to subset the data - taking into account
  # raw or mean data

  selected_data_vars_chd(
    var_chd = input$var_chd,
    subset_outcome_chd = subsetOutcomeChd(),
    rate_count = input$rate_count_chd
  )
})


keepVars <- reactive({
  if (isTotals()) {
    c("year")
  } else if (isSex()) {
    c("sex", "year")
  } else if (isAge()) {
    c("age_adult", "year")
  } else {
    c("age_adult", "sex", "year", "id_var")
  }
})


dataObj <- reactive({
  keep.vars <- c(keepVars(), selectedDataVarsChd())
  x <- copy(subsetOutcomeChd()[, ..keep.vars])
  
  # Rename value variables
  x.names <- colnames(x)
  inx1 <- grep("rate|count", x.names)
  colnames(x)[inx1] <- prettyVarChdUnits()
  x
})

toFactor <- reactive({
  to_factor_chd(
    dat = dataObj(),
    is_totals = isTotals(),
    is_sex = isSex(),
    is_age = isAge(),
    plot_var_id = plotVarId(),
    ui_sex_levels = ui_sex_levels
  )
})

# PLOTLY -----------------------------------------------------------
plotVarId <- reactive({
  
  if (isTotals()) {
    c("1")
  } else if (isSex()) {
    c("sex")
  } else if (isAge()) {
    c("age_adult")
  } else {
    c("sex", "age_adult")
  }
})

plotlyObj <- reactive({
  make_plotly_chd(
    x = toFactor(),
    plot_title = dataTitle(),
    num_digits = numDigits(),
    thousands_sep = thousands_sep,
    dec_mark = dec_mark,
    is_totals = isTotals(),
    is_sex = isSex(),
    is_age = isAge(),
    pretty_var_chd_units = prettyVarChdUnits(),
    replace_outcome_string_chd = replaceOutcomeStringChd()
  )
})



# DATATABLES --------------------------------------------------------------

dtCastChd <- reactive({
  # One dcast for both rates and counts
  dt_cast_chd(
    x = dataObj(),
    pretty_var_chd_units = prettyVarChdUnits(),
    is_totals = isTotals(),
    is_sex = isSex(),
    is_age = isAge(), ui_year = ui_year, ui_sex_levels = ui_sex_levels
  )
})

numDigits <- reactive({
  digits = 1
  if(input$rate_count_chd == 1){
    digits = 0
  }
  digits
})

outputDT_chd <- reactive({
  
  x <- copy(dtCastChd())
  n_col <- NCOL(x)
  makeRateDT_chd(
    x,
    group_var = ui_year,
    dt_title = "H",
    messageBottom = "B",
    n_col = n_col,
    digits = numDigits()
  )
  
})

# VALIDATE ----------------------------------------------------------------

isTotals <- reactive({
  input$agCHD == "totals"
})

isSex <- reactive({
  input$agCHD == "sex"
})

isAge <- reactive({
  input$agCHD == "age"
})

isAgeSex <- reactive({
  
  input$agCHD == "age_sex"
})

isChd <- reactive({
  input$bar == "chd"
})

isValidSelection <- reactive({
  !all( (isAge()|isAgeSex()), input$var_chd == "count_n_incidence", input$oCHD == 3)
})



# RENDER ------------------------------------------------------------------

output$d3_chd <- renderPlotly({
  req(input$var_chd, isValidSelection())
  
  plotlyObj()
})

output$table_counts_chd <- renderDT({
  req(input$var_chd, isValidSelection())
  outputDT_chd()
})


output$invalid_selection_text <- renderText({
  if(isValidSelection()){
    ""
  }  else {
  HTML(paste0("<h3>", ui_warning_invalid_selection, "</h3>"))
  }
})

# This ensures that the "invalid_selection_text object is always available in the browser - and so available to the conditionalPanel fn()
# https://github.com/rstudio/shiny/issues/1318
outputOptions(output, "invalid_selection_text", suspendWhenHidden = FALSE)