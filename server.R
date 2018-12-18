library(shiny)
library(r2d3)
library(DT)
library(magrittr)

shinyServer(function(input, output, session) {
  options(DT.options = list(
    pageLength = 20,
    dom = "Bt",
    buttons = c('copy', 'csv', 'pdf')
  ))
  load(file = "data/shiny_dat.rda")
  load(file = "data/export_med.Rdata")
  outcome_descriptions <-
    fread(file = "documentation/outcome_descriptions.csv", encoding = "UTF-8")
  #
  
  # outcome_descriptions <-
  #   fread(file = "data/descriptions.csv", encoding = "UTF-8")
  #
  source("ui-dk.R", encoding = "UTF-8")
  
  output$outcome_title <- renderText({
    input$outcome
  })
  output$outcome_description <- renderText({
    outcome_descriptions[outcome == input$outcome, description_dk]
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
    } else if (any(outcome_names_diag %in% input$outcome)) {
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
    # Outputs same character string that's used in the UI input field
    data_var_names <- selectedDataVars()
    
    c(variable_names[code_name == data_var_names[1], var_dk], variable_names[code_name == data_var_names[2], var_dk])
    
    
  })
  
  
  
  # SUBSETTING ------------------------------------------------------
  subsetOutcome <- reactive({
    # Cache subset based on outcome, aggr level, and theme
    if (input$aggr_level != "national") {
      shiny_dat[[outcomeCode()]][[input$aggr_level]]
    } else {
      # No real reason to pick age - but need any dataset (not edu) that will be
      # aggregated
      shiny_dat[[outcomeCode()]]$age
    }
    
  })
  
  selectedDataVars <- reactive({
    grep(input$variable, colnames(subsetOutcome()), value = TRUE)
  })
  subsetVars <- reactive({
    dat <- subsetOutcome()
    col_vars <- c("year", "sex", "grouping", selectedDataVars())
    dat <- dat[, ..col_vars]
    colnames(dat) <-
      c(ui_year, ui_sex, prettyAggr_level(), prettyVariable())
    dat[]
  })
  subsetYear <- function()
    ({
      # Subset the already partially subset data based on years
      
      dat <- subsetVars()[get(ui_year) == input$year,]
      dat[]
    })
  
  
  # FORMATTING DATA ---------------------------------------------------------
  outputCasesData <- reactive({
    # National level data shows all years
    # This is not a reactive, because then it somehow turns the "<10" strings
    # into 0s. The reactive wasn't being called when I thought it was
    if (input$aggr_level != "national") {
      dat <- subsetYear()
      dat[, (ui_year) := NULL]
    } else {
      subsetYear()
    }
    
  })
  
  outputCasesD3Line <- reactive({
    # Replace value.var with reactive that corresponds to the variable the user selected
    # TODO: Allow switching between rates and counts for plot
    
    dat <-
      dcast(
        subsetVars(),
        get(ui_year) ~ get(ui_sex),
        value.var = prettyVariable()[1],
        fun.aggregate = sum
      )
    colnames(dat) <-
      c(ui_year, "female", "male") # TODO: needs to be language agnostic
    dat[, variable := prettyVariable()[1]]
    
  })
  
  outputCasesD3Bar <- reactive({
    # Restrict data to the user selected vairable, and give pretty column names
    # browser()
    keep_cols <- c(ui_sex, prettyAggr_level(), prettyVariable())
    dat <- subsetYear()[, ..keep_cols]
    dat[, (prettyVariable()) := lapply(.SD, function(i) {
      # Any NA values need to be converted to 0s to be sent to d3
      i[is.na(i)] <- 0
      i
    }),
    .SDcols = prettyVariable()]
    
    dat[, 1:3]
    #TODO: Allow switching between rates and counts for plot
  })
  
  
  plot_d3_bar <- reactive({
    if (nrow(outputCasesD3Bar()) > 0  &
        input$aggr_level != "national") {
      simpleD3Bar(data = outputCasesD3Bar())
    }
    
  })
  
  
  plot_d3_line <- reactive({
    if (input$aggr_level == "national") {
      simpleD3Line(data = outputCasesD3Line())
    }
  })
  
  plot_d3_legend <- reactive({
    sex_vars <- ui_sex_levels
    color = c("#bd6916", "#166abd")
    data = data.frame(sex = sex_vars,
                      color =  color)
    simpleD3Legend(data)
    
    
  })
  
  # DATATABLES --------------------------------------------------------------
  outputCountDTTable <- reactive({
    # Organizes data for DataTable outputs. Needs to be characters, and need
    # flag column to color men/women rows
    dat <- copy(outputCasesData())
    group_var <- prettyAggr_level()
    dat[, prettyVariable()[2] := NULL]
    dat <-  dcast(
      dat,
      get(group_var) ~ get(ui_sex),
      value.var = prettyVariable()[1],
      fun.aggregate = sum
    )
    dat[, Total := rowSums(dat[, .(female, male)])]
    
    dat <- dat[, lapply(.SD, function(i) {
      i[is.na(i)] <- "<10"
      i
    })]
    
    colnames(dat) <- c(group_var, ui_sex_levels, "Total")
    
    DT::datatable(
      data = dat,
      extensions = 'Buttons',
      rownames = FALSE,
      class = ' hover row-border',
      options = list(
        buttons = c('copy', 'pdf'),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#e7e7e7'});",
          "}"
        )
      )
    ) %>%
      formatStyle('Total',  fontWeight = 'bold') %>%
      formatStyle(group_var,  backgroundColor = "#e7e7e7")
    
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
    dat <- dat[, lapply(.SD, function(i) {
      i[is.na(i)] <- "<10"
      i
    })]
    
    colnames(dat) <- c(group_var, ui_sex_levels)
    
    DT::datatable(
      data = dat,
      extensions = 'Buttons',
      rownames = FALSE,
      class = 'hover row-border',
      options = list(
        buttons = c('copy', 'pdf'),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#e7e7e7'});",
          "}"
        )
      )
    ) %>%
      formatStyle(group_var,  backgroundColor = "#e7e7e7")
  })
  
  # VALIDATE BEFORE PLOTING -------------------------------------------------
  validate <- reactive({
    all(input$outcome != "", input$year > 0, input$theme != "")
  })
  
  
  # CHANGE UI BASED ON INPUTS -----------------------------------------------
  observe({
    shinyjs::toggle(id = "theme", condition = input$outcome != "")
    shinyjs::toggle(id = "year", condition = input$theme != "")
    shinyjs::toggle(id = "variable", condition = input$theme != "")
    shinyjs::toggle(id = "aggr_level", condition = input$theme != "")
  })
  
  
  choiceYears <- reactive({
    # User can only select years >=2009 when viewing regional data
    if (input$aggr_level == "region") {
      return(c(2009:2015))
    } else {
      return(c(2006:2015))
    }
    
  })
  observe({
    updateSelectInput(
      session = session,
      inputId = "year",
      choices = choiceYears(),
      selected = 2015
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
  
  output$d3_plot_legend <- renderSimpleD3Legend({
    if (validate()) {
      plot_d3_legend()
    }
  })
  
  # DATATABLES:
  # AGE
  output$table <- renderDT({
    if (validate()) {
      outputCountDTTable()
    }
  }, server = FALSE)
  
  output$table_margins <- renderDT({
    if (validate()) {
      outputRateDTTable()
    }
  }, server = FALSE)
  
  
})