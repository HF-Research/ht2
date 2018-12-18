library(shiny)
library(shinyWidgets)
library(data.table)
library(r2d3)
library(shinyBS)
library(lubridate)
library(DT)

devtools::install_github('matthew-phelps/simpled3', force = FALSE)
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
             selectInput(
               inputId = "outcome",
               label = choose_outcome,
               choices = outcome_choices,
               selectize = TRUE
             ),
             fluidRow(column(
               7,
               radioGroupButtons(
                 inputId = "variable",
                 label = choose_var,
                 choices = variable_choices_opr,
                 justified = TRUE,
                 direction = "vertical",
                 individual = FALSE
                 
               )
             ),
             column(
               5,
               selectInput(
                 inputId = "year",
                 label = choose_year,
                 choices = NULL,
                 selected = 2015
               ),
             fluidRow(
             radioGroupButtons(
                 inputId = "aggr_level",
                 label = choose_aggr_lv,
                 choices = aggr_choices,
                 justified = TRUE,
                 direction = "vertical"
               )
             )
             
           ))
           )
           ),
    
    column(
      id = "col_description",
      7,
      wellPanel(
        class = "well_description",
        h2(textOutput("outcome_title")),
        hr(),
        textOutput("outcome_description")
      )
    )
  ),
  
  fluidRow(
    column(
      id = "col_output",
      12,
      align = "center",
      br(),
      
      # Plots
      fluidRow(
        conditionalPanel(
          condition = "input.aggr_level != 'national'",
          simpleD3BarOutput("d3_plot_bar")
        ),
        conditionalPanel(
          "input.aggr_level == 'national'",
          
          simpleD3LineOutput("d3_plot_line_html")
        )
      ),
      fluidRow(simpleD3LegendOutput("d3_plot_legend", height = '50px')),
      
      br(),
      br(),
      
      # DataTables
      fluidRow(
        column(6, DTOutput("table")),
        column(6, DTOutput("table_margins"))
        ),
      fluidRow(br(), br())
    
    )
  )
)