
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

prettyVarChdUnits <- reactive({
  x <- prettyVarChd()
  x[c("name_count", "name_rate")][as.integer(input$rate_count_chd)]
})

prettyVarChd <- reactive({
  x <-
    unlist(var_descriptions_chd[code_name == input$var_chd, .(name, name_count, name_rate)])
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
    
    
    title_text <- tags$b(title_text)
    
    col_selection <- "desc_general"
    desc_text <-
      var_descriptions_chd[code_name == input$var_chd, ..col_selection]
    
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
  tmp <-
    grep(var_stripped, colnames(subsetOutcomeChd()), value = TRUE)
  tmp[as.integer(input$rate_count_chd)]
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
  x <- copy(dataObj())
  
  # Turn characters into factors
  if (isTotals()) {
    x
  } else if (isSex()) {
    x[, sex := factor(sex,
                      c(levels = "f", "m"),
                      labels = c(ui_sex_levels[1], ui_sex_levels[2]))]
    x[, id_var := paste0(get(plotVarId()[1]))]
  } else if (isAge()) {
    x[, id_var := paste0(get(plotVarId()[1]))]
    } else {
    x[, sex := factor(sex,
                      c(levels = "f", "m"),
                      labels = c(ui_sex_levels[1], ui_sex_levels[2]))]
    
  }
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
  
  x <- toFactor()
  plot_title <- paste0(input$outcome_chd, " ", prettyVarChd()[1])
  
  axis_font_size <- 20
  legend_font_size <- 17
  tick_font_size <- 14
  linesize = 3
  pointsize = 8
  tooltip <-
    paste0(prettyVarChdUnits(), ": <br> <b> %{y:.1f} </b>")
  
  if (isTotals()) {
    
    tooltip <-
      paste0(prettyVarChdUnits(), ": <br> <b> %{y:.1f} </b><extra></extra>")
    
   out <-  plot_ly(data = x, x = ~ year) %>%
      add_trace(
        y = ~ get(prettyVarChdUnits()),
        type = 'scatter',
        mode = 'lines+markers',
        color = I(single_val_col),
        line = list(width = linesize),
        marker = list(size = pointsize),
        hovertemplate = tooltip
      )
    
    
    
  } else if (isSex()) {
    out <- plot_ly(data = x, x = ~ year) %>%
      add_trace(
        y = ~ get(prettyVarChdUnits()),
        color = ~ id_var,
        colors = rev(graph_colors),
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = linesize),
        marker = list(size = pointsize),
        hovertemplate = tooltip
      )
  } else if (isAge()) {
    
    out <- plot_ly(data = x, x = ~ year) %>%
      add_trace(
        y = ~ get(prettyVarChdUnits()),
        linetype = ~ age_adult,
        color = I(single_val_col),
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = linesize),
        marker = list(size = pointsize),
        hovertemplate = tooltip
      )
  } else {
    # tmp1 <- rep(ui_sex_levels[1], 2)
    # tmp2 <- rep(ui_sex_levels[2], 2)
    # legend.labs <- paste0(c(tmp1, tmp2), c(" <15", " 15+"))
    # legend.cols <- rep(graph_colors, each = 2)
    #
    setorder(x, year)
    out <- plot_ly(data = x) %>%
      add_trace(
        x = ~ year,
        y = ~ get(prettyVarChdUnits()),
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
    xaxis = list(title = list(
      text = ui_year,
      font = list(size = axis_font_size)
    ),
    tickfont = list(size = tick_font_size)),
    yaxis = list(
      title = list(text = prettyVarChdUnits(),
                   font = list(size = axis_font_size)),
      tickfont = list(size = tick_font_size),
      rangemode = "tozero"),
    hoverlabel = list(font = list(size = 18)),
    hovermode = "x"
    
  ) %>%
    config(
      locale = "da",
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
    ) %>% 
    layout(separators = paste0(dec_mark, thousands_sep),
           legend = list(
             font = list(size = legend_font_size)))
})

# DATATABLES --------------------------------------------------------------

dtCastChd <- reactive({
  # One dcast for both rates and counts
  
  dat <- dataObj()
  if (isTotals()) {
    setnames(dat, c(ui_year, "Total"))
    dat
  } else if (isSex()) {
    value_var <- prettyVarChdUnits()
    subset_cols = c("year", value_var)
    out = cbind(dat[sex == "f", ..subset_cols], dat[sex == "m", ..value_var])
    setnames(out, c(ui_year, ui_sex_levels[1], ui_sex_levels[2]))
  } else if (isAge()) {
    value_var <- prettyVarChdUnits()
    subset_cols = c("year", value_var)
    out = cbind(dat[age_adult == "<18", ..subset_cols], dat[age_adult == "18+", ..value_var])
    setnames(out, c(ui_year, "<18", "18+"))
  } else {
    
    # Id_var created, and key set, in data pre-processing step 
    value_var <- prettyVarChdUnits()
    subset_cols = c("year", value_var)
    out <-
      cbind(dat["f<18", ..subset_cols],
            dat["f18+", ..value_var],
            dat["m<18", ..value_var],
            dat["m18+", ..value_var])
    
    tmp1 <- rep(ui_sex_levels[1], 2)
    tmp2 <- rep(ui_sex_levels[2], 2)
    var_names <- paste0(c(tmp1, tmp2), c(" <18", " 18+"))
    setnames(out, c(ui_year, var_names))
    out
  }
  
  
  
})


outputDT_chd <- reactive({
  digits = 1
  
  if(input$rate_count_chd == 1){
    
    digits = 0
  }
  
  x <- copy(dtCastChd())
  n_col <- NCOL(x)
  makeRateDT_chd(
    x,
    group_var = ui_year,
    dt_title = "H",
    messageBottom = "B",
    n_col = n_col,
    digits = digits
  )
  
})

# VALIDATE ----------------------------------------------------------------

isTotals <- reactive({
  input$aggr_level_chd == "totals"
})

isSex <- reactive({
  input$aggr_level_chd == "sex"
})

isAge <- reactive({
  input$aggr_level_chd == "age"
})

isAgeSex <- reactive({
  
  input$aggr_level_chd == "age_sex"
})

isChd <- reactive({
  input$navbar == "chd"
})

isValidSelection <- reactive({
  !all( (isAge()|isAgeSex()), input$var_chd == "count_n_incidence", outcomeCodeChd() == 3)
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