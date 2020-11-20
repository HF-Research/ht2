tabPanel(ui_main_title,
         value = "cvd",
         useShinyjs(),
         
         
         fluidRow(
           div(
             # This changes the column width (i.e. proportion) based on width of screen)
             class = "col-xs-12 col-sm-5 col-md-4 col-lg-3",
             
             wellPanel(class = "well_input",
                       fluidRow(
                         selectInput(
                           inputId = "oCVD",
                           label = choose_outcome,
                           choices = outcome_choices,
                           selectize = TRUE
                         )
                       ),
                       fluidRow(
                         column(
                           6,
                           # This UI has to change based on "outcome" choice
                           fluidRow(uiOutput("varChoices")),
                           fluidRow(
                             selectInput(
                               inputId = "year",
                               label = choose_year,
                               choices = year_max,
                               selected = year_max
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
               
               # Graph panel
               tabPanel(
                 title = ui_d3_figures,
                 
                 fluidRow(
                   class = "data_warning",
                   column(
                     12,
                     
                     align = "left",
                    textOutput("data_warning_graph")
                   )
                 ),
                 br(),
                 conditionalPanel(condition = "input.agCVD != 'national'",
                                  (
                                    plotlyOutput("d3_plot_bar", height = "600px")
                                  )),
                 conditionalPanel(condition = "input.agCVD == 'national'",
                                  (
                                    plotlyOutput("plotly_line_cvd", height = "600px")
                                  )),
                 
                 fluidRow(column(
                   12, align = "left",
                   uiOutput("rate_count_desc"),
                   br()
                 )),
                 fluidRow(br(), br())
               ),
               
               # Map Panel
               tabPanel(
                 title = ui_map,
                 fluidRow(
                   class = "data_warning_map",
                   column(
                     12,
                     
                     align = "left",
                     textOutput("data_warning")
                   )
                 ),
                 fluidRow(
                   class = "row_outcome_title",
                   column(
                     12,
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
                     # (plotlyOutput(
                     #   "map_male", width = 420, height = 550
                     # )),
                     (leafletOutput(
                       "map_male", width = 420, height = 550
                     )),
                     fluidRow(align = "center", textOutput("map_title_male")),
                     fluidRow(
                       downloadButton(
                         outputId = "downloadMapsMale",
                         label = paste0(ui_download, " ", ui_sex_levels[2]),
                         class = "btn radiobtn btn-default",
                       )
                     )
                   ),
                   column(
                     class = "col_leaflet",
                     6,
                     align = "left",
                     (leafletOutput(
                       "map_female", width = 420, height = 550
                     )),
                     fluidRow(align = "center", (textOutput("map_title_female"))),
                     fluidRow(
                       downloadButton(
                         outputId = "downloadMapsFemale",
                         class = "btn radiobtn btn-default",
                         label = paste0(ui_download, " ", ui_sex_levels[1])
                         
                       )
                     )
                   ),
                   br()
                 ),
                 
                 fluidRow(align = "left",
                          uiOutput("rate_desc_map"),
                          br())
                 
               ),
               
               # Data panel
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
               )
             ),
             
             br()
             
             
           )
         ))