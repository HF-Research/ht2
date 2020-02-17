# callModule(profvis_server, "profiler")
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

prettyVarChd <- reactive({
  x <- unlist(var_choices_chd[code_name == input$var_chd, .(var_dk, var_rate_dk)])
  x[as.integer(input$rate_count_chd)]
})



outcomeCodeChd <- reactive({
  # Connect the input in normal language to the hjertetal_code. This is so we
  # can change the description without having to rename allll the datasets.
  
  outcome_descriptions_chd[name_dk == input$outcome_chd, ht.code]
})




# DATA MUNGING --------------------------------------------------------------
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


keepVars <- reactive({
  if(isTotals()) {
    c("year")
  } else if (isSex()) {
    c("sex", "year")
  } else {
    c("age_adult", "sex", "year")
  }
})





dataObj <- reactive({
  keep.vars <- c(keepVars(), selectedDataVarsChd())
  x <- copy(subsetOutcomeChd()[, ..keep.vars])
  
  # Rename value variables
  x.names <- colnames(x)
  inx1 <- grep("incidence", x.names)
  if (any(inx1)) {
    colnames(x)[inx1] <- prettyVarChd()
  } else {
    inx2 <- grep("prevalence", x.names)
    colnames(x)[inx2] <- prettyVarChd()
  }
  x
})

toFactor <- reactive({
  x <- copy(dataObj())
  # Turn characters into factors
  if (isTotals()) {
    x
  } else if (isSex()) {
    x[, sex := factor(sex,
                      c(levels = "f", "m"),
                      labels = c(ui_sex_levels[1], ui_sex_levels[2]))]
    x[, id_var := paste0(get(plotVarId()[1]))]
  } else {
    x[, sex := factor(sex,
                      c(levels = "f", "m"),
                      labels = c(ui_sex_levels[1], ui_sex_levels[2]))]
    x[, id_var := paste0(get(plotVarId()[1]), " ", get(plotVarId()[2]))]
  }
})

# FORMAT FOR plotly -----------------------------------------------------------
plotVarId <- reactive({
  if (isTotals()) {
    c("1")
  } else if (isSex()) {
    c("sex")
  } else {
    c("sex", "age_adult")
  }
})

plotlyObj <- reactive({
  x <- toFactor()
  color = c(graph_colors[1], graph_colors[2])
  
  y.var <- sym(prettyVarChd())
  color.var <- sym(plotVarId()[1])
  ymax <- x[, max(get(prettyVarChd()))]
  linesize = 1.1
  pointsize = 2.9
  tooltip <-
    paste0(prettyVarChd(), ": <br> <b> %{y} </b> <extra></extra>")
  
  if (isTotals()) {
    
    out <- plot_ly(data = x, x = ~ year) %>%
      add_trace(y = ~ get(prettyVarChd()),
                type = 'scatter',
                mode = 'lines+markers',
                hovertemplate = tooltip)
      
    
    
  } else if (isSex()) {
    
    out <- plot_ly(data = x, x = ~year) %>%
      add_trace(y = ~ get(prettyVarChd()),
                color = ~id_var,
                colors = graph_colors,
                type = 'scatter',
                mode = 'lines+markers',
                hovertemplate = tooltip) 
      } else {
    tmp1 <- rep(ui_sex_levels[1], 2)
    tmp2 <- rep(ui_sex_levels[2], 2)
    legend.labs <- paste0(c(tmp1, tmp2), c(" <15", " 15+"))
    legend.cols <- rep(graph_colors, each = 2)
    
    setorder(x, year)
    out <- plot_ly(data = x ) %>%
      add_trace(
        x = ~ year,
        y = ~ get(prettyVarChd()),
        color = ~ sex,
        colors = graph_colors,
        linetype = ~ age_adult,
        type = 'scatter',
        mode = 'lines+markers',
        hovertemplate = tooltip
        
      )
          
      }
  
  out %>% layout(
    yaxis = list(title = prettyVarChd(),
                 rangemode = "tozero"),
    hoverlabel = list(font = list(size = 18))
  ) %>%
    config(
      modeBarButtonsToRemove = c(
        "zoomIn2d",
        "zoomOut2d",
        "zoom2d",
        "pan2d",
        "select2d",
        "lasso2d",
        "autoScale2d",
        "resetScale2d"
      )
    )
})


# DATATABLES --------------------------------------------------------------

dtCastChd <- reactive({
  # One dcast for both rates and counts
  
  dat <- dataObj()
  if(isTotals()){
    dat
  } else if (isSex()){
    value_var <- prettyVarChd()
    subset_cols = c("year", value_var)
    out = cbind(dat["f", ..subset_cols], dat["m", ..value_var])
    setnames(out, c("year", ui_sex_levels[1], ui_sex_levels[2]))
  } else {
    dat[, id_var := paste0(sex, age_adult)]
    value_var <- prettyVarChd()
    subset_cols = c("year", value_var)
    out <-
      cbind(dat[id_var == "f<15", ..subset_cols],
            dat[id_var == "f15+", ..value_var],
            dat[id_var == "m<15", ..value_var],
            dat[id_var == "m15+", ..value_var])
    
    tmp1 <- rep(ui_sex_levels[1], 2)
    tmp2 <- rep(ui_sex_levels[2], 2)
    var_names <- paste0(c(tmp1, tmp2), c(" <15", " 15+"))
    setnames(out, c("year", var_names))
    out
  }
  
  
  
})


outputDT_chd <- reactive({
 x <- copy(dtCastChd())

 makeRateDT_chd(x, group_var = "year", dt_title = "H", messageBottom = "B")
  
  })  

# VALIDATE ----------------------------------------------------------------

isTotals <- reactive({
  input$aggr_level_chd == "totals"
})

isSex <- reactive({
  input$aggr_level_chd == "sex"
})

# RENDER ------------------------------------------------------------------

output$d3_chd <- renderPlotly({
  req(input$var_chd)
  
  plotlyObj()
           
  
})

output$table_counts_chd <- renderDT({
  
  req(input$var_chd)
      outputDT_chd()
  
})
