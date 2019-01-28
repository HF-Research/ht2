library(shiny)
library(shinyWidgets)
library(data.table)
library(shinyBS)
library(lubridate)
library(DT)

# devtools::install_github('matthew-phelps/simpled3', force = TRUE)
library(simpled3)


source(file = "ui-dk.R", encoding = "UTF-8")

max_year <- 2015
ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css-ht2.css")
  ),
  titlePanel("HjerteTal2"),
  fluidRow(
    column(id = "col_input",
           4,
           wellPanel(
             class = "well_input",
             selectInput(
               inputId = "outcome",
               label = choose_outcome,
               choices = outcome_choices,
               selectize = TRUE
             ),
             fluidRow(column(7,
                             # This UI has to change based on "outcome" choice
                             uiOutput("varButtonChoices")),
                      column(
                        5,
                        radioGroupButtons(
                          inputId = "aggr_level",
                          label = choose_aggr_lv,
                          choices = aggr_choices,
                          justified = TRUE,
                          direction = "vertical"
                        ),
                        fluidRow(
                          selectInput(
                            inputId = "year",
                            label = choose_year,
                            choices = NULL,
                            selected = 2015
                          )
                        )
                        
                      ))
           )),
    
    column(
      id = "col_description",
      7,
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
  
  fluidRow(
    column(
      id = "col_output",
      12,
      align = "center",
      fluidRow(
        conditionalPanel(condition = "input.aggr_level != 'national'",
                         simpleD3BarOutput("d3_plot_bar")),
        conditionalPanel(
          "input.aggr_level == 'national'",
          simpleD3LineOutput("d3_plot_line_html")
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
      
      fluidRow(
        column(12, align = "left",
               uiOutput("rate_count_desc"),
               br())
      ),
      
      
      fluidRow(column(12, align = "left",
                      h3("Data"))),
      # DataTables
      fluidRow(
        column(6,
               fluidRow(tags$b(
                 textOutput("table1_title")
               )),
               fluidRow(DTOutput("table"))),
        column(6,
               fluidRow(tags$b(
                 textOutput("table2_title")
               )),
               fluidRow(DTOutput("table_margins")))
      ),
      fluidRow(br(), br())
      
    )
  )
)
