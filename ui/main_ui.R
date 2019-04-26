tabPanel(ui_main_title,
         
         tags$head(
           tags$link(rel = "stylesheet", type = "text/css", href = "www/css-ht2.css")
         ),
         fluidRow(
           # Column for inputs
           column(
             id = "col_input",
             4,
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
                         column(7,
                                # This UI has to change based on "outcome" choice
                                fluidRow(uiOutput("varButtonChoices")),
                                fluidRow(
                                  selectInput(
                                    inputId = "year",
                                    label = choose_year,
                                    choices = NULL,
                                    selected = 2015
                                  )
                                )),
                         column(
                           5,
                           radioGroupButtons(
                             inputId = "aggr_level",
                             label = choose_aggr_lv,
                             choices = aggr_choices,
                             justified = TRUE,
                             direction = "vertical"
                           )
                           
                           
                         )
                       )),
             # DESCRITPIONS
             fluidRow(
               profvis_ui("profiler"),
               wellPanel(
                 class = "well_description",
                 textOutput("outcome_title"),
                 hr(),
                 uiOutput("outcome_description"),
                 br(),
                 uiOutput("variable_desc")
                 
               )
             )
           ),
           
           # RESULTS
           column(
             id = "col_output",
             8,
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
                                  ))
               ),
               tabPanel(
                 title = ui_data,
                 fluidRow(
                   class = "row_outcome_title",
                   column(
                     11,
                     offset = 1,
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
                     )))
                   ),
                   column(
                     class = "col_DT",
                     6,
                     align = "center",
                     fluidRow(tags$b(textOutput("table2_title"))),
                     fluidRow(((
                       DTOutput("table_rates")
                     )))
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
                 fluidRow(column(
                   class = "col_leaflet",
                   6,
                   align = "left",
                   withSpinner(leafletOutput("map_male", width = 420, height = 550)),
                   textOutput("map_title_male")
                 ),
                 column(
                   class = "col_leaflet",
                   6,
                   align = "left",
                   withSpinner(leafletOutput("map_female", width = 420, height = 550)),
                   textOutput("map_title_female")
                 ),
                 br())
               )
             ),
             
             br(),
             
             conditionalPanel(
               condition = "output.tabs",
               fluidRow(
                 column(
                   id = "col_rate_count",
                   3,
                   align = "left",
                   radioGroupButtons(
                     inputId = "count_rates",
                     label =  NULL,
                     choices = count_rate_choices,
                     justified = TRUE
                   )
                 ),
                 conditionalPanel(condition = "output.tabFigure",
                                  column(8,
                                    align = "right",
                                         uiOutput("downloadButton"))),
                 conditionalPanel(condition = "output.tabMap",
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
                                  ))
               ),
               fluidRow(column(
                 12, align = "left",
                 uiOutput("rate_count_desc"),
                 br()
               )),
               fluidRow(br(), br())
             )
           )
         ))