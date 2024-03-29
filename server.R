shinyServer(function(input, output, session) {
  # source("global.R")
  # session$onSessionEnded(stopApp)
  
  source(file.path("server", "main_server.R"),
         encoding = "UTF-8",
         local = TRUE)$value
  source(file.path("server", "chd_server.R"), local = TRUE)$value
  # source(file.path("server", "risk_predict_server.R"), local = TRUE)$value
  source(file.path("server", "about_server.R"), local = TRUE)$value
  source(file.path("server", "about_chd_server.R"), local = TRUE)$value
  

  # POPUP IE WARNING --------------------------------------------------------
  
  observeEvent(label = "IEwarning",  input$check, {
    if (input$check == "TRUE") {
      showModal(
        modalDialog(
          title = "HjerteTal does not work with Internet Explorer",
          easyClose = TRUE,
          fade = TRUE,
          tags$p(
            "Please choose Chrome / Firefox / Safari / Edge"
          )
          
        )
      )
    }
  })
  
  # BOOKMARKING -------------------------------------------------------------
  
  always_exclude <- c(
    "check",
    # "agCHD",
    # "agCVD",
    # "year",
    "rate_count",
    "rate_count_chd",
    "about_selection_chd",
    "data_vi
    s_tabs_chd",
    "downloadMapsFemale_bttn",
    "downloadMapsMale_bttn",
    "data_vis_tabs",
    "download_line",
    "table_rates_rows_selected",
    "table_counts_rows_selected",
    "table_rates_rows_current",
    "table_rates_rows_all",
    "table_rates_search",
    "table_counts_cell_clicked",
    "table_rates_cell_clicked",
    "table_rates_rows_all",
    "table_counts_rows_current",
    "download_bar",
    "map_male_zoom",
    "map_male_zoom",
    "map_male_center",
    "downloadMapsMale_bttn",
    "download_line",
    "map_female_center",
    "map_female_bounds",
    "map_male_bounds",
    "plotly_hover-A",
    "plotly_afterplot-A",
    "table_counts_chd_rows_all",
    "table_counts_chd_cell_clicked",
    "table_counts_chd_rows_selected",
    "table_counts_chd_state",
    "table_counts_chd_search",
    "table_counts_chd_rows_current",
    "table_counts_chd_rows_all",
    "table_faq_state",
    "table_diag_search",
    "table_diag_rows_all",
    "table_faq_rows_current",
    "table_diag_rows_selected",
    "table_diag_cell_clicked",
    "table_faq_cell_clicked",
    "table_faq_chd_rows_current",
    "table_diag_chd_search",
    "table_faq_chd_cell_clicked",
    "table_diag_chd_rows_current",
    "table_edu_cell_clicked",
    "table_edu_rows_current",
    "table_ethnicity_state",
    "table_med_state",
    "table_opr_rows_selected",
    "table_opr_rows_selected",
    "table_opr_state",
    "table_ethnicity_rows_current",
    "table_ethnicity_rows_selected",
    "table_pop_rows_current",
    "table_pop_rows_selected",
    "table_edu_state",
    "table_pop_cell_clicked",
    "table_pop_rows_all",
    "table_ethnicity_cell_clicked",
    "table_opr_rows_current",
    "table_edu_rows_all",
    "table_faq_cell_clicked",
    "table_med_rows_selected",
    "table_edu_search",
    "table_ethnicity_rows_all",
    "table_pop_search",
    "table_pop_state",
    "table_ethnicity_search",
    "table_opr_search",
    "table_opr_cell_clicked",
    "table_med_search",
    "table_edu_rows_selected",
    "table_med_rows_current",
    "table_diag_chd_cell_clicked",
    "table_med_rows_all",
    "table_opr_rows_all",
    "table_med_cell_clicked",
    "table_counts_search",
    "table_counts_rows_all",
    "table_counts_state",
    "table_rates_state",
    "map_female_zoom",
    ".clientValue-default-plotlyCrosstalkOpts",
    "table_faq_rows_all",
    "table_faq_rows_selected",
    "table_faq_search",
    "table_diag_rows_current",
    "table_diag_state",
    "table_faq_chd_state",
    "table_diag_chd_rows_selected",
    "table_diag_chd_state",
    "table_diag_chd_rows_all",
    "table_faq_chd_rows_selected",
    "table_faq_chd_rows_all",
    "table_faq_chd_search",
    "table_diag_row_last_clicked",
    "table_counts_chd_row_last_clicked",
    "table_faq_chd_row_last_clicked",
    "table_faq_chd_row_last_clicked",
    "table_rates_row_last_clicked",
    "table_counts_row_last_clicked",
    "table_diag_chd_row_last_clicked",
    'table_faq_row_last_clicked',
    "table_med_row_last_clicked",
    "table_edu_row_last_clicked",
    "table_ethnicity_row_last_clicked",
    "table_counts_columns_selected",
    "table_rates_cells_selected",
    "table_counts_cells_selected",
    "table_rates_columns_selected",
    "table_counts_chd_cells_selected",
    "table_counts_chd_columns_selected",
    "table_pop_row_last_clicked",
    'map_male_shape_mouseover',
    "map_male_shape_mouseout",
    "map_female_shape_mouseout",
    "map_female_shape_mouseover",
    "table_opr_row_last_clicked",
    "plotly_relayout-A",
    "table_faq_columns_selected",
    "table_faq_cells_selected",
    "table_updates_rows_selected",
    "table_updates_search",
    "table_updates_state",
    "table_updates_rows_all",
    "table_updates_columns_selected",
    "table_updates_rows_current",
    "table_updates_cell_clicked",
    "table_updates_cells_selected",
    "table_diag_cells_selected",
    "table_diag_cells_selected",
    "table_diag_columns_selected",
    "table_opr_columns_selected",
    "table_opr_cells_selected",
    "table_med_columns_selected",
    "table_med_cells_selected",
    "table_edu_cells_selected",
    "table_edu_columns_selected",
    "table_ethnicity_cells_selected",
    "table_ethnicity_columns_selected",
    "table_pop_columns_selected",
    "table_pop_cells_selected"
  )
  
  barChange <- reactive({input$bar})
  
  # Everything below only fires when barCahnge() is invalidates (i.e. when
  # input$bar changes)
  observeEvent(barChange(),label = "BkmrkExclude", {
    if (input$bar == "cvd") {
      bookmarkingWhitelist <- c("bar", "varCVD", "oCVD", "agCVD", "year")
    } else if (input$bar == "chd") {
      bookmarkingWhitelist <- c("bar", "oCHD", "var_chd", "agCHD")
    } else if (input$bar == "helpCVD") {
      bookmarkingWhitelist <- c("bar", "about_selection")
    } else if (input$bar == "helpCHD") {
      bookmarkingWhitelist <- c("bar")
    }
    # browser()
    
    toExclude <- setdiff(names(input), bookmarkingWhitelist)
    toExclude <- c(always_exclude, toExclude)
    setBookmarkExclude(toExclude)
    
    
    
  })
  
  toListen <- reactive(label = "bkmrkListen", {
    # Put any events that are to be bookmarked in here. These events should NOT
    # be in the always_exclude() list
    req(input$varCVD) # This stops multiple bookmark setting during initialization 
    list(input$bar,
         input$oCVD,
         input$varCVD,
         input$agCVD,
         input$year,
         input$oCHD,
         input$agCHD,
         input$var_chd,
         input$about_selection)
  })
  observeEvent(toListen(), label = "doBookmark", {
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
  })
