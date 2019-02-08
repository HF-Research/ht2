tabPanel(
  ui_about_title,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css-ht2.css")
  ),
  fluidRow(column(
    3,
    class = "about_well",
    radioGroupButtons(
      inputId = "about_selection",
      label = about_selection,
      choices = about_choices,
      justified = TRUE,
      direction = "vertical"
    )
    
  ), 
  column(9,
         fluidRow(h3(textOutput("ui_about_title"))),
         fluidRow(textOutput("ui_about_desc"),
                  br()),
         fluidRow(textOutput("ui_about_desc_2"),
                  br())
  )),
  conditionalPanel(condition = "input.about_selection == 'def_diag'",
                   fluidRow(column(
                     11, align = "center",
                     DTOutput("table_diag")
                   ))),
  conditionalPanel(condition = "input.about_selection == 'def_opr'",
                   
                   fluidRow(column(
                     11, align = "center",
                     DTOutput("table_opr")
                   ))),
  conditionalPanel(condition = "input.about_selection == 'def_med'",
                   
                   fluidRow(column(
                     11, align = "center",
                     DTOutput("table_med")
                   ))),
  conditionalPanel(condition = "input.about_selection == 'def_variables'"),
  
  conditionalPanel(condition = "input.about_selection == 'def_populations'"),
  conditionalPanel(condition = "input.about_selection == 'def_stratas'")
  
)