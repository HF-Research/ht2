
ui <- navbarPage(

  title = "HjerteTal2",
  source(file.path("ui", "main_ui.R"), local = TRUE)$value,
  source(file.path("ui", "about_ui.R"), local = TRUE)$value,
  useShinyjs()
)
