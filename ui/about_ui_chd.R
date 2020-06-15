tabPanel(
  ui_about_title_chd,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css-ht2.css")
  ),
  fluidRow(
    column(
      3,
      class = "about_well",
      radioGroupButtons(
        inputId = "about_selection_chd",
        label = about_selection_chd,
        choices = about_choices_chd,
        justified = TRUE,
        direction = "vertical"
      )
      
    ),
    column(
      9,
      class = "col_about_text",
      fluidRow(h3(textOutput("ui_about_title_chd"))),
      fluidRow(htmlOutput("ui_about_desc_chd"),
               br()),
      fluidRow(textOutput("ui_about_desc_2_chd")
               ),
      conditionalPanel(condition = "input.about_selection_chd == 'general'",
                       fluidRow(
                         column(
                           class = "col_about_dt",
                           12,
                           align = "center",
                           DTOutput("table_faq_chd")
                         )
                       )), 
      br(),
      
      conditionalPanel(condition = "input.about_selection_chd == 'def_diag'",
                       fluidRow(
                         column(
                           class = "col_about_dt",
                           12,
                           align = "center",
                           DTOutput("table_diag_chd")
                         )
                       ))
    )
  ),
  
  
  
  br(),
  br(),
  br()
)