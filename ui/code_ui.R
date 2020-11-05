tabPanel(
  ui_code_title,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css-app-specifc.css")
  ),
  fluidRow(class = "col-xs-12 col-sm-10 col-md-8 col-lg-6",
           includeMarkdown("documentation/r_code.md"))
)




