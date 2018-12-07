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
  load(file = "data/export_summaries_opr.Rdata")
  source("ui-dk.R", encoding = "UTF-8")
  outcome_descriptions <-
    fread(file = "documentation/outcome_descriptions.csv", encoding = "UTF-8")
  
  output$outcome_title <- renderText({
    input$outcome
  })
  output$outcome_description <- renderText({
    outcome_descriptions[outcome == input$outcome, description_dk]
  })
  
  # DYNAMIC VARIABLES/COLUMN NAMES ------------------------------------------
  dtColNames <- reactive({
    # Adapt DT column names to the appropriate dataset.
    switch(
      input$aggr_level,
      "age" = ui_colnames_cases_age,
      "edu" = ui_colnames_cases_edu,
      "region" = ui_colnames_cases_region,
      "national" = ui_colnames_cases_national
    )
  })
  
  outcomeGroup <- reactive({
    # Define which columns are in the outputed dataset.
    if (any(outcome_names_treatment %in% input$outcome)) {
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
    switch(
      outcomeGroup(),
      "treatment" = names(which(
        variable_choices_opr == input$variable
      )),
      "diag" = names(which(
        variable_choices_diag == input$variable
      )),
      "med" = names(which(
        variable_choices_med == input$variable
      ))
    )
    
  })
  
  
  
  # REACTIVE FUNCTIONS ------------------------------------------------------
  subsetOutcome <- reactive({
    # Cache subset based on outcome, aggr level, and theme
    export[[input$outcome]][[input$aggr_level]][[input$theme]]
  })
  subsetYear <- reactive({
    # Subset the already partially subset data based on years
    subsetOutcome()[year == input$year,]
    
  })
  
  outputCasesData <- reactive({
    # National level data shows all years
    if (input$aggr_level != "national") {
      dat <- subsetYear()
      dat <-
        dat[, .(sex,
                grouping,
                n_patients,
                n_oprs,
                n_dead_30,
                n_dead_1yr)]
      
    } else if (input$aggr_level == "national") {
      dat <- subsetOutcome()
      dat <- dat[, .(sex,
                     year,
                     n_patients,
                     n_oprs,
                     n_dead_30,
                     n_dead_1yr)]
    }
    dat[]
  })
  
  outputCasesD3Line <- reactive({
    # Replace value.var with reactive that corresponds to the variable the user selected
    dat <-
      dcast(outputCasesData(), year ~ sex, value.var = input$variable)
    dat[, variable := prettyVariable()]
  })
  
  outputCasesD3Bar <- reactive({
    # Restrict data to the user selected vairable, and give pretty column names
    
    dat <- outputCasesData()
    vars_holding_data <- sapply(variable_choices_opr, function(i)
      i)
    dat[, (vars_holding_data) := lapply(.SD, function(i) {
      # Any NA values need to be converted to 0s to be sent to d3
      i[is.na(i)] <- 0
      i
    }),
    .SDcols = vars_holding_data]
    
    colnames(dat) <- dtColNames()
    keep_cols <- c("Sex", prettyAggr_level(), prettyVariable())
    dat <- dat[, keep_cols, with = FALSE]
    dat[]
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
    colors = data.frame(male = "#166abd",
                        female = "#bd6916")
    simpleD3Legend(colors = colors)
    
    
  })
  
  outputCasesDTTable <- reactive({
    # Organizes data for DataTable outputs. Needs to be characters, and need
    # flag column to color men/women rows
    dat <- outputCasesData()
    dat <- dat[, lapply(.SD, as.character)]
    dat <- dat[, lapply(.SD, function(i) {
      i[is.na(i)] <- "<10"
      i
    })]
    
    # Make invisible column so we can color male and females
    colnames(dat) <- dtColNames()
    dat[, flag := 1]
    dat[Sex == "female", flag := 0]
    
    # Make sure "flag" variable is always first column, so we can
    # programatically reference it
    out_names <- colnames(dat)
    out_names <-
      c(out_names[length(out_names)], out_names[-length(out_names)])
    
    setcolorder(dat, neworder = out_names)
    
    DT::datatable(
      data = dat,
      extensions = 'Buttons',
      rownames = FALSE,
      options = list(columnDefs = list(list(
        visible = FALSE, targets = 0
      )))
    ) %>%
      formatStyle(
        "Sex",
        "flag",
        color = "white",
        fontWeight = "bold",
        backgroundColor = styleInterval(c(0), c("#bd6916", "#166abd"))
      )
    
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
      outputCasesDTTable()
    }
  }, server = FALSE)
  
  output$table_margins <- renderDT({
    if (validate()) {
      outputCasesDTTable()
    }
  }, server = FALSE)
  
  
})