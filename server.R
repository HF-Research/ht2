shinyServer(function(input, output, session) {
  # source("global.R")
  # session$onSessionEnded(stopApp)
  
  source(file.path("server", "main_server.R"),
         encoding = "UTF-8",
         local = TRUE)$value
  source(file.path("server", "chd_server.R"), local = TRUE)$value
  source(file.path("server", "about_server.R"), local = TRUE)$value
  source(file.path("server", "about_chd_server.R"), local = TRUE)$value

# BOOKMARKING -------------------------------------------------------------
setBookmarkExclude(
    c(
      "data_vis_tabs_chd",
      "downloadMapsFemale_bttn",
      "rate_count",
      "downloadMapsMale_bttn",
      "about_selection_chd",
      "about_selection",
      "year",
      "data_vis_tabs",
      "rate_count_chd",
      "check",
      "download_line",
      "aggr_level_chd",
      "aggr_level",
      "variable",
      "var_chd",
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
      "table_pop_row_last_clicked",
      'map_male_shape_mouseover',
      "map_male_shape_mouseout",
      "map_female_shape_mouseout",
      "map_female_shape_mouseover",
      "table_opr_row_last_clicked",
      
      "plotly_relayout-A")
  )
  
  
  
  toListen <- reactive({
    # Put any events that are to be bookmarked in here. These events should NOT
    # ben in the setBookmarkExclude() list
    list(input$bar,
    input$oCVD,
    input$oCHD,
    input$varCVD)
  })
  observeEvent(toListen(), {
    
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
  
})
