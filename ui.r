ui <- fluidPage(
  div(style = "padding-left: -15px; padd-right: -15px;",
      titlePanel(
        title = "", windowTitle = "HjerteTal"
      )),
  navbarPage(
    title = div(img(
      src = "hf-logo.png",
      style = "margin-top: -10px;",
      height = 40
    )),
    collapsible = TRUE,
    source(file.path("ui", "main_ui.R"), local = TRUE)$value,
    source(file.path("ui", "about_ui.R"), local = TRUE)$value
  )
)
