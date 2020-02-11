
# TEXT --------------------------------------------------------------------


output$outcome_description_chd <- renderUI({
  req(input$outcome_chd)
  
  out_title <- tags$b(input$outcome_chd)
  out <-
    outcome_descriptions_chd[hjertetal_code == outcomeCodeChd(), .(desc_dk, link_dk)]
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





outcomeCodeChd <- reactive({
  # Connect the input in normal language to the hjertetal_code. This is so we
  # can change the description without having to rename allll the datasets.
  
  outcome_descriptions_chd[name_dk == input$outcome_chd, ht.code]
})




# SUBSETTING --------------------------------------------------------------
subsetOutcomeChd <- reactive({
  # Cache subset based on outcome
  subsetAggr()[[outcomeCodeChd()]]
})

subsetAggr <- reactive({
  shiny_dat_chd[[input$aggr_level_chd]]
})

selectedDataVarsChd <- reactive({
  # Returns the column names to be used to subset the data - taking into account
  # raw or mean data
  var_stripped <- gsub("count_n_|rate_", "", input$var_chd)
  tmp <- grep(var_stripped, colnames(subsetOutcomeChd()), value = TRUE)
  tmp[as.integer(input$rate_count_chd)]
})



subsetYearChd <- reactive({
  subsetVars()[get(ui_year) == input$year,][, (ui_year) := NULL]
})


keepVars <- reactive({
  if(input$aggr_level_chd == "age") {
    c("ht.code", "age_adult", "sex")
  } else if (input$aggr_level_chd == "sex") {
    c("ht.code", "sex")
  } else {
    "ht.code"
  }
  
})


# FORMAT FOR D3 -----------------------------------------------------------
outputCasesDataChd <- function() {
  # National level data shows all years
  if (!isTotals()) {
    subsetYear()
  } else {
    subsetVars()
  }
}

d3PlotLineChd <- reactive({
  
    sex_vars <- ui_sex_levels
    color = c(graph_colors[1], graph_colors[2])
    keep.vars <- c(keepVars(), selectedDataVarsChd())
    x <- subsetOutcomeChd()[, ..keep.vars]
    
    # plot_title = plotTitle()
    ggplot(
      data = x,
      colors = c(graph_colors[1], graph_colors[2]),
      plotTitle = plot_title,
      sexVars = sex_vars
    )
  
})


# VALIDATE ----------------------------------------------------------------

isTotals <- reactive({
  input$aggr_level == "Totals"
})


# RENDER ------------------------------------------------------------------

output$d3_chd <- renderSimpleD3Line({
  browser()
  req(input$var_chd)
  
  d3PlotLineChd()
  
})

