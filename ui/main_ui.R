tabPanel(ui_main_title,
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
                           inputId = "outcome",
                           label = choose_outcome,
                           choices = outcome_choices,
                           selectize = TRUE
                         )
                       ),
                       fluidRow(
                         column(6,
                                # This UI has to change based on "outcome" choice
                                fluidRow(uiOutput("varChoices")),
                                fluidRow(
                                  selectInput(
                                    inputId = "year",
                                    label = choose_year,
                                    choices = NULL,
                                    selected = 2015
                                  )
                                ),
                                fluidRow(
                                  radioGroupButtons(
                                    inputId = "rate_count",
                                    label =  choose_rate_count,
                                    choices = count_rate_choices,
                                    justified = TRUE
                                  )
                                )
                                ),
                         column(6,
                                # This UI has to change based on "outcome" & "var" choice
                                uiOutput("aggrButtonChoices"))
                       )),
             # DESCRITPIONS
             fluidRow(
               # profvis_ui("profiler"),
               wellPanel(
                 class = "well_description",
                 uiOutput("outcome_description"),
                 br(),
                 uiOutput("variable_desc")
                 
               )
             )
           ),
           
           
           # RESULTS
           div(
             class = "col-xs-12 col-sm-7 col-md-8 col-lg-9",
             align = "right",
             tabsetPanel(
               id = "data_vis_tabs",
               type = "pill",
               
               
               tabPanel(
                 title = ui_d3_figures,
                 br(),
                 conditionalPanel(condition = "input.aggr_level != 'national'",
                                  (
                                    simpleD3BarOutput("d3_plot_bar", height = "550px")
                                  )),
                 conditionalPanel(condition = "input.aggr_level == 'national'",
                                  (
                                    simpleD3LineOutput("d3_plot_line_html", height = "550px")
                                  )),
                 fluidRow(
                   div(
                     class = "col-xs-6 col-sm-6 col-md-6 col-lg-9",
                     id = "download_button_row",
                     align = "left",
                     uiOutput("downloadButton")
                   )
                 ),
                 fluidRow(column(
                   12, align = "left",
                   uiOutput("rate_count_desc"),
                   br()
                 )),
                 fluidRow(br(), br())
               ),
               tabPanel(
                 title = ui_data,
                 fluidRow(
                   class = "row_outcome_title",
                   column(
                     11,
                     class = "output_titles",
                     align = "left",
                     textOutput("outcome_title_dt")
                   )
                 ),
                 fluidRow(
                   class = "row_outcome_title",
                   column(
                     class = "col_DT",
                     6,
                     align = "center",
                     fluidRow(tags$b(textOutput("table1_title"))),
                     fluidRow(((
                       DTOutput("table_counts")
                     ))),
                     fluidRow(align = "left",
                              uiOutput("count_desc"),
                              br())
                   ),
                   column(
                     class = "col_DT",
                     6,
                     align = "center",
                     fluidRow(tags$b(textOutput("table2_title"))),
                     fluidRow(((
                       DTOutput("table_rates")
                     ))),
                     fluidRow(align = "left",
                              uiOutput("rate_desc"),
                              br())
                   )
                 )
               ),
               
               tabPanel(
                 title = ui_map,
                 fluidRow(
                   class = "row_outcome_title",
                   column(
                     11,
                     offset = 1,
                     class = "output_titles",
                     align = "left",
                     textOutput("outcome_title_map")
                   )
                 ),
                 fluidRow(
                   column(
                     class = "col_leaflet",
                     6,
                     align = "left",
                     withSpinner(leafletOutput(
                       "map_male", width = 420, height = 550
                     )),
                     fluidRow(align = "center", textOutput("map_title_male"))
                   ),
                   column(
                     class = "col_leaflet",
                     6,
                     align = "left",
                     withSpinner(leafletOutput(
                       "map_female", width = 420, height = 550
                     )),
                     fluidRow(align = "center", (textOutput("map_title_female")))
                   ),
                   br()
                 ),
                 fluidRow(
                   column(
                       9,
                     align = "right",
                     downloadBttn(
                       "downloadMapsMale",
                       label = paste0("Hente ", ui_sex_levels[2]),
                       size = "sm"
                     ),
                     downloadBttn(
                       "downloadMapsFemale",
                       label = paste0("Hente ", ui_sex_levels[1]),
                       size = "sm"
                     )
                   )
                 ),
                 fluidRow(align = "left",
                          uiOutput("rate_desc_map"),
                          br())
               )
             ),
             
             br()
             
             # conditionalPanel(
             #   condition = "output.tabFigure",
             #   fluidRow(
             #     div(
             #       class = "col-xs-6 col-sm-6 col-md-6 col-lg-9",
             #       align = "left",
             #       uiOutput("downloadButton")
             #     )
             #   ),
             #   fluidRow(column(
             #     12, align = "left",
             #     uiOutput("rate_count_desc"),
             #     br()
             #   )),
             #   fluidRow(br(), br())
             # )
           )
         ))