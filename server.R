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
      "var_chd"
      )
  )
  observe({
    req(input$variable)
    input$navbar
    input$var_chd
    input$output
    
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
  
})
