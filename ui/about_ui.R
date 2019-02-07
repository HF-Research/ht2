tabPanel(ui_about_title,
         tags$head(
           tags$link(rel = "stylesheet", type = "text/css", href = "css-ht2.css")
         ),
         fluidRow(column(
           11,
           
             class = "about_well",
             radioGroupButtons(
               inputId = "about_selection",
               label = about_selection,
               choices = about_choices,
               justified = TRUE
               
             )
          
           )
         ),
         conditionalPanel(condition = "input.about_selection == 'def_diag'",
                          fluidRow(textOutput(def_diag_title)),
                          fluidRow(column(11, align = "center",
                                          DTOutput("table_diag")))
         ),
         conditionalPanel(condition = "input.about_selection == 'def_opr'",
                          fluidRow(textOutput(def_opr_title)),
                          fluidRow(column(11, align = "center",
                                          DTOutput("table_opr")))
         ),
         conditionalPanel(condition = "input.about_selection == 'def_med'",
                          fluidRow(textOutput(def_med_title)),
                          fluidRow(column(11, align = "center",
                                          DTOutput("table_med")))
         ),
         conditionalPanel(condition = "input.about_selection == 'def_variables'",
                          textOutput(def_variables_title)),
         conditionalPanel(condition = "input.about_selection == 'def_populations'",
                          textOutput(def_population_title)),
         conditionalPanel(condition = "input.about_selection == 'def_stratas'",
                          textOutput(def_stratas_title))
         
)