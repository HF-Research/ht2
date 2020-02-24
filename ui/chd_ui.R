tabPanel(ui_chd_title,
         useShinyjs(),
         
         tags$head(
           tags$link(rel = "stylesheet", type = "text/css", href = "www/css-ht2.css")
         ),
         fluidRow(
           # profvis_ui("profiler"),
           div(# This changes the column width (i.e. proportion) based on width of screen)
             class = "col-xs-12 col-sm-5 col-md-4 col-lg-3",
             
             wellPanel(
               class = "well_input",
               fluidRow(
                 selectInput(
                   inputId = "outcome_chd",
                   label = choose_outcome_chd,
                   choices = outcome_choices_chd,
                   selectize = TRUE,
                   selected = ""
                 )
               ),
               fluidRow(
                 column(
                   6,
                   # This UI has to change based on "outcome" choice
                   selectInput(
                     inputId = "var_chd",
                     label = choose_var_chd,
                     choices = ui_var_choices_chd,
                     selectize = TRUE
                   ),
                   fluidRow(
                     radioGroupButtons(
                       inputId = "rate_count_chd",
                       label =  choose_rate_count,
                       choices = count_rate_choices,
                       justified = TRUE
                     )
                   )
                   
                 ),
                 column(
                   6,
                   radioGroupButtons(
                     inputId = "aggr_level_chd",
                     label = choose_aggr_lv_chd,
                     choiceNames =  aggr_levels_chd_pretty,
                     choiceValues = aggr_levels_chd,
                     justified = TRUE,
                     direction = "vertical",
                     selected = "totals"
                   )
                 )
               )
             ),
             
             # DESCRIPTION TEXT
             fluidRow(
               # profvis_ui("profiler"),
               wellPanel(
                 class = "well_description",
                 uiOutput("outcome_description_chd"),
                 br(),
                 uiOutput("variable_desc_chd")
                 
               )
             )
             ),
           
           
           
           # RESULTS ---------------------------
           # Graph
           div(
             class = "col-xs-12 col-sm-7 col-md-8 col-lg-9",
             align = "right",
             tabsetPanel(
               id = "data_vis_tabs_chd",
               type = "pill",
               
               tabPanel(
                 title = ui_d3_figures,
                 br(),
                 plotlyOutput("d3_chd", height = "600px"),
                 fluidRow(
                   div(class = "col-xs-6 col-sm-6 col-md-6 col-lg-9",
                       id = "download_button_row_chd",
                       align = "left",
                       # uiOutput("downloadButton_chd"))
                   ),
                   
                   # Buttons and text after graph
                   fluidRow(column(12, align = "left",
                                   # uiOutput("rate_count_desc_chd"),
                                   br())),
                   fluidRow(br(), br())
                 )),
                 
                 
                 # Data panel
                 tabPanel(
                   title = ui_data,
                   fluidRow(
                     class = "row_outcome_title",
                     column(
                       11,
                       class = "output_titles",
                       align = "left"
                       # textOutput("outcome_title_dt_chd")
                     )
                   ),
                   fluidRow(
                     class = "row_outcome_title",
                     column(
                       class = "col_DT",
                       6,
                       align = "center",
                       # fluidRow(tags$b(textOutput("table1_title"))),
                       fluidRow(((
                         DTOutput("table_counts_chd")
                       ))),
                       fluidRow(align = "left",
                                # uiOutput("count_desc_chd"),
                                br())
                     )
                     # column(
                     #   class = "col_DT",
                     #   6,
                     #   align = "center",
                     #   fluidRow(tags$b(textOutput("table2_title_chd"))),
                     #   fluidRow(((
                     #     DTOutput("table_rates_chd")
                     #   ))),
                     #   fluidRow(align = "left",
                     #            uiOutput("rate_desc_chd"),
                     #            br())
                     # )
                   )
                 )


               )
               
             )
           )
         )

         