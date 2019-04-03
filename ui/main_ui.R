tabPanel(
  ui_main_title,
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css-ht2.css")
  ),
  fluidRow(
    column(id = "col_input",
           5,
           wellPanel(
             class = "well_input",
             fluidRow(
               selectInput(
                 inputId = "outcome",
                 label = choose_outcome,
                 choices = outcome_choices,
                 selectize = TRUE
               )
             ),
             fluidRow(column(
               7,
               # This UI has to change based on "outcome" choice
               fluidRow(uiOutput("varButtonChoices")),
               fluidRow(
                 selectInput(
                   inputId = "year",
                   label = choose_year,
                   choices = NULL,
                   selected = 2015
                 )
               )
             ),
             column(
               5,
               radioGroupButtons(
                 inputId = "aggr_level",
                 label = choose_aggr_lv,
                 choices = aggr_choices,
                 justified = TRUE,
                 direction = "vertical"
               )
               
               
             ))
           )),
    
    column(
      id = "col_description",
      7,
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
  
  fluidRow(column(
    id = "col_output",
    12,
    align = "center",
    tabsetPanel(
      id = "data_vis_tabs",
      tabPanel(
        ui_d3_figures,
        conditionalPanel(condition = "input.aggr_level != 'national'",
                         simpleD3BarOutput("d3_plot_bar", height = "550px")),
        conditionalPanel(
          condition = "input.aggr_level == 'national'",
          simpleD3LineOutput("d3_plot_line_html", height = "550px")
        )
      ),
      
      tabPanel("Data",
               fluidRow(
                 column(6,
                        fluidRow(tags$b(
                          textOutput("table1_title")
                        )),
                        fluidRow(withSpinner(DTOutput("table_counts")))),
                 column(6,
                        fluidRow(tags$b(
                          textOutput("table2_title")
                        )),
                        fluidRow(withSpinner(DTOutput("table_rates"))))
               )
               ),
      
      tabPanel(ui_map,
               fluidRow(
                 column(12, align = "left",
                        withSpinner(combineWidgetsOutput("maps", height = 550))),
                
               br())
               )
    ),
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
      column(3,
             offset = 6,
             uiOutput("downloadButton"))
    ),
    fluidRow(column(
      12, align = "left",
      uiOutput("rate_count_desc"),
      br()
    )),
    fluidRow(br(), br())
  )
  
  # fluidRow(column(12, align = "left",
  #                 h3("Data")))
  # # DataTables)
  )
)