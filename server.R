library(shiny)
library(r2d3)
library(DT)
library(magrittr)

shinyServer(function(input, output) {
  
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
  
  subsetOutcome <- reactive({
    # Cache subset based on outcome, aggr level, and theme
    export[[input$outcome]][[input$aggr_level]][[input$theme]]
  })
  subsetYear <- reactive({
    # Subset the already partially subset data based on years
    subsetOutcome()[year == input$year,]
    
  })
  
  selectVars <- reactive({
    c("sex",
      "grouping",
      "n_patients",
      "n_oprs",
      "n_dead_30",
      "n_dead_1yr")
    
  })
  dtColNames <- reactive({
    # Adapt DT column names to the appropriate dataset. Need to at switch() here probably
    out <- ui_colnames_cases
    if (input$aggr_level != "national") {
      out <- out[-1]
    }
    out
  })
  
  outputCasesTable <- reactive({
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
    
    colnames(dat) <- dtColNames()
    dat <- dat[, lapply(.SD, as.character)]
    dat <- dat[, lapply(.SD, function(i) {
      i[is.na(i)] <- "<10"
      i
    })]
    
    out <- DT::datatable(dat,
                  extensions = 'Buttons') %>% formatStyle(
                    "Sex",
                    target = "row",
                    backgroundColor = styleEqual(c("female", "male"), c("snow", "#e9f6fe"))
                  )
    
  })
  
  # AGE
  output$table_age <- renderDT({outputCasesTable()}, server = FALSE)
  
  # EDU
  output$table_edu <- renderDT({outputCasesTable()}, server = FALSE)
  
  
  # REGION
  output$table_region <- renderDT({outputCasesTable()}, server = FALSE)
  
  # NATIONAL
  output$table_national <- renderDT({outputCasesTable()}, server = FALSE)
  
  
  
})