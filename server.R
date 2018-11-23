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
  
  
  # REACTIVE FUNCTIONS ------------------------------------------------------
  subsetOutcome <- reactive({
    # Cache subset based on outcome, aggr level, and theme
    export[[input$outcome]][[input$aggr_level]][[input$theme]]
  })
  subsetYear <- reactive({
    # Subset the already partially subset data based on years
    subsetOutcome()[year == input$year,]
    
  })
  
  selectVars <- reactive({
    # Can't remember why this is needed
    
    out <- c("year",
             "sex",
             "grouping",
             "n_patients",
             "n_oprs",
             "n_dead_30",
             "n_dead_1yr")
    if (input$aggr_level != "national") {
      out <- out[-1]
    }
    out
  })
  
  
  dtColNames <- reactive({
    # Adapt DT column names to the appropriate dataset. Need to at switch() here probably
    
    out <- switch(
      input$aggr_level,
      "age" = ui_colnames_cases_age,
      "edu" = ui_colnames_cases_edu,
      "region" = ui_colnames_cases_region,
      "national" = ui_colnames_cases_national
    )
    
    out
  })
  
  outputCasesData <- reactive({
    # National level data shows all years
    
    if (input$aggr_level != "national") {
      dat <- subsetYear()
      dat <-
        dat[, .(sex, grouping, n_patients, n_oprs, n_dead_30, n_dead_1yr)]
      
    } else if (input$aggr_level == "national") {
      dat <- subsetOutcome()
      dat <-
        dat[, .(year,
                sex,
                grouping,
                n_patients,
                n_oprs,
                n_dead_30,
                n_dead_1yr)]
    }
    
    
    dat[]
  })
  
  outputCasesD3Line <- reactive({
    # Replace value.var with reactive that corresponds to the variable the user selected
    dcast(outputCasesData(), year ~ sex, value.var = input$variable)
  })
  
  outputCasesD3Bar <- reactive({
    # Replace value.var with reactive that corresponds to the variable the user selected
    dat <- outputCasesData()
    
    keep_cols <- c("sex", "grouping", input$variable)
    dat <- dat[, keep_cols, with = FALSE]
    # colnames(dat) <- c("sex", "grouping", "value")
    dat[]
  })
  
  outputCasesDTTable <- reactive({
    dat <- outputCasesData()
    
    dat <- dat[, lapply(.SD, as.character)]
    dat <- dat[, lapply(.SD, function(i) {
      i[is.na(i)] <- "<10"
      i
    })]
    
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
      options = list(columnDefs = list(list(
        visible = FALSE, targets = 1
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
  
  
  plot_d3 <- reactive({
    
    if (nrow(outputCasesD3Bar()) > 0  & input$aggr_level != "national") {
      
      r2d3(data = outputCasesD3Bar(), script = "bar.js")
    }
    
  })
  
  plot_d3_line <- reactive({
    if (input$aggr_level == "national") {
      r2d3(data = outputCasesD3Line(), script = "line.js")
    }
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
  
  
  
  # RENDER FUNCTIONS --------------------------------------------------------
  
  # PLOT
  output$d3_plot <- renderD3({
    plot_d3()
    
  })
  
  output$d3_plot_line <- renderD3({
    plot_d3_line()
    
    
  })
  
  # DATATABLES:
  # AGE
  output$table_age <- renderDT({
    outputCasesDTTable()
  }, server = FALSE)
  
  # EDU
  output$table_edu <- renderDT({
    outputCasesDTTable()
  }, server = FALSE)
  
  # REGION
  output$table_region <-
    renderDT({
      outputCasesDTTable()
    }, server = FALSE)
  
  # NATIONAL
  output$table_national <-
    renderDT({
      outputCasesDTTable()
    }, server = FALSE)
  
  
  
})