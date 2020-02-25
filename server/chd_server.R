# TEXT --------------------------------------------------------------------


output$outcome_description_chd <- renderUI({
  req(input$outcome_chd)
  
  out_title <- tags$b(input$outcome_chd)
  keep_vars <- c("desc", "link")
  out <-
    outcome_descriptions_chd[ht.code == outcomeCodeChd(), ..keep_vars]
  # Add link for further reading - if link exists, otherwise just desc
  url <- a(ui_read_more,
           href = (out$link),
           target = "_blank")
  
  if (!is.na(out$link)) {
    tagList(out_title, out$desc, url)
  }
  else {
    tagList(out_title, out$desc)
  }
})

prettyVarUnits <- reactive({
  x <- prettyVarChd()
  x[c("name_count", "name_rate")][as.integer(input$rate_count_chd)]
})

prettyVarChd <- reactive({
  
  x <- unlist(var_descriptions_chd[code_name == input$var_chd, .(name, name_count, name_rate)])
  x
})

outcomeCodeChd <- reactive({
  # Connect the input in normal language to the hjertetal_code. This is so we
  # can change the description without having to rename allll the datasets.
  
  outcome_descriptions_chd[name == input$outcome_chd, ht.code]
})


replaceOutcomeStringChd <- reactive({
  replace_outcome_string <- input$outcome_chd
  # Lowercase first character only (keeps abbreviations in caps)
  substr(replace_outcome_string, 1, 1) <-
    tolower(substr(replace_outcome_string, 1, 1))
  replace_outcome_string
})


output$variable_desc_chd <- renderUI({
  req(input$var_chd, input$outcome_chd,
      selectedDataVarsChd())
  
  isolate({
    # Append title to front of variable descr text
    
    title_text <- prettyVarChd()[1]
    
    if(lang == "dk"){
    title_text <- (paste0("Den ", tolower(title_text)))
    }
    title_text <- tags$b(title_text)
    
    col_selection <- "desc_general"
    desc_text <-
      var_descriptions_chd[code_name == selectedDataVars()[1], ..col_selection]
    
    # Replace sections of variable desc that are specific for
    # outcome/year/outcome-type
    
    desc_text <-
      gsub(
        "REPLACE_OUTCOME",
        replaceOutcomeStringChd(),
        (desc_text$desc_general),
        fixed = TRUE
      )
    # For some reason, some danish characters encoding is messed up after the
    # gsub fn(). This fixes that
    
    desc_text <- gsub("Ã¥", "å", desc_text, fixed = TRUE)
    desc_text <- gsub("Any CHD", ui_replace_all_chd, desc_text)
      
    tagList(title_text, (desc_text))
    
  })
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
    colnames(x)[inx1] <- prettyVarUnits()
  } else {
    inx2 <- grep("prevalence", x.names)
    colnames(x)[inx2] <- prettyVarUnits()
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

# PLOTLY -----------------------------------------------------------
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
  plot_title <- paste0(input$outcome_chd, " ", prettyVarChd()[1])
  
  axis_font_size <- 20
  linesize = 3
  pointsize = 12
  tooltip <-
    paste0(prettyVarUnits(), ": <br> <b> %{y} </b>")
  
  if (isTotals()) {
    
    out <- plot_ly(data = x, x = ~ year) %>%
      add_trace(y = ~ get(prettyVarUnits()),
                type = 'scatter',
                mode = 'lines+markers',
                hovertemplate = tooltip,
                line = list(width = linesize),
                marker = list(size = pointsize))
      
    
    
  } else if (isSex()) {
    
    out <- plot_ly(data = x, x = ~year) %>%
      add_trace(y = ~ get(prettyVarUnits()),
                color = ~id_var,
                colors = rev(graph_colors),
                type = 'scatter',
                mode = 'lines+markers',
                line = list(width = linesize),
                marker = list(size = pointsize),
                hovertemplate = tooltip) 
      } else {
    # tmp1 <- rep(ui_sex_levels[1], 2)
    # tmp2 <- rep(ui_sex_levels[2], 2)
    # legend.labs <- paste0(c(tmp1, tmp2), c(" <15", " 15+"))
    # legend.cols <- rep(graph_colors, each = 2)
    # 
    setorder(x, year)
    out <- plot_ly(data = x ) %>%
      add_trace(
        x = ~ year,
        y = ~ get(prettyVarUnits()),
        color = ~ sex,
        colors = rev(graph_colors),
        linetype = ~ age_adult,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = linesize),
        marker = list(size = pointsize),
        hovertemplate = tooltip
        
      )
          
      }

  out %>% layout(
    margin = list(t = 60),
    title = list(
      text = plot_title,
      x = 0,
      yanchor = "bottom",
      
      font = list(family = c("Roboto"),
                  size = 25)
    ),
    xaxis = list(title = list(text = ui_year,
                              font = list(size = axis_font_size))),
    yaxis = list(title = list(text = prettyVarUnits(),
                              font = list(size = axis_font_size)),
                 rangemode = "tozero"
  ),
    hoverlabel = list(font = list(size = 18)),
    hovermode = "x"
    
  ) %>%
    config(
      locale = "no",
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
    value_var <- prettyVarUnits()
    subset_cols = c("year", value_var)
    out = cbind(dat["f", ..subset_cols], dat["m", ..value_var])
    setnames(out, c("year", ui_sex_levels[1], ui_sex_levels[2]))
  } else {
    dat[, id_var := paste0(sex, age_adult)]
    value_var <- prettyVarUnits()
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