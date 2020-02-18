shinyServer(function(input, output, session) {
  # source("global.R")
  # session$onSessionEnded(stopApp)
  
  source(file.path("server", "main_server.R"),
         encoding = "UTF-8",
         local = TRUE)$value
  source(file.path("server", "chd_server.R"), local = TRUE)$value
  source(file.path("server", "about_server.R"), local = TRUE)$value
  source(file.path("server", "about_chd_server.R"), local = TRUE)$value
  
})
