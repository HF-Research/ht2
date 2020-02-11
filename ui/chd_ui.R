tabPanel(ui_chd_title,
         useShinyjs(),
         
         tags$head(
           
           tags$link(rel = "stylesheet", type = "text/css", href = "www/css-ht2.css")
         ),
         fluidRow(
           div(
             # This changes the column width (i.e. proportion) based on width of screen)
             class = "col-xs-12 col-sm-5 col-md-4 col-lg-3",
             
             wellPanel(class = "well_input",
                       fluidRow(
                         selectInput(
                           inputId = "outcome_chd",
                           label = choose_outcome_chd,
                           choices = outcome_choices_chd,
                           selectize = TRUE
                         )
                       ),
                       fluidRow(
                         column(6,
                                # This UI has to change based on "outcome" choice
                                selectInput(
                                  inputId = "var_chd",
                                  label = choose_var_chd,
                                  choices = var_choices_chd,
                                  selectize = TRUE
                                ),
                                fluidRow(
                                  selectInput(
                                    inputId = "year",
                                    label = choose_year,
                                    choices = year_choices_chd,
                                    selected = year_max_chd
                                  )
                                )
                         )
                       )
             )
           )
         )
)
