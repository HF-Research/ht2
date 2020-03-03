tabPanel(
  ui_about_title,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css-ht2.css")
  ),
  fluidRow(
    column(
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
    column(
      9,
      class = "col_about_text",
      fluidRow(h3(textOutput("ui_about_title"))),
      fluidRow(textOutput("ui_about_desc"),
               br()),
      fluidRow(textOutput("ui_about_desc_2"),
               br())
    )
  ),
  br(),
 
  
   conditionalPanel(condition = "input.about_selection == 'def_diag'",
                   fluidRow(
                     column(
                       class = "col_about_dt",
                       12,
                       align = "center",
                       DTOutput("table_diag")
                     )
                   )),
  conditionalPanel(condition = "input.about_selection == 'def_opr'",
                   
                   fluidRow(
                     column(class = "col_about_dt",
                            12, align = "center",
                            DTOutput("table_opr"))
                   )),
  conditionalPanel(condition = "input.about_selection == 'def_med'",
                   
                   fluidRow(
                     column(class = "col_about_dt",
                            12, align = "center",
                            DTOutput("table_med"))
                   )),
  conditionalPanel(condition = "input.about_selection == 'def_variables'"),
  
  conditionalPanel(condition = "input.about_selection == 'def_populations'",
                   fluidRow(
                     column(class = "col_about_dt col-xs-12 col-sm-7 col-md-6 col-lg-4",
                            12, align = "center",
                            DTOutput("table_pop"))
                   )),
  conditionalPanel(condition = "input.about_selection == 'def_edu'",
                   fluidRow(
                     column(class = "col_about_dt",
                            12, align = "center",
                            DTOutput("table_edu"))
                   )),
  conditionalPanel(condition = "input.about_selection == 'def_ethnicity'",
                   fluidRow(
                     column(class = "col_about_dt",
                            12, align = "center",
                            DTOutput("table_ethnicity"))
                   )),
  br(),
  br(),
  br()
)