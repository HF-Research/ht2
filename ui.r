library(shiny)
library(shinyWidgets)
library(shinyjs)
library(data.table)
library(ggplot2)
library(r2d3)
library(shinyBS)
library(lubridate)
library(DT)

devtools::install_github('matthew-phelps/simpled3', force = TRUE)
library(simpled3)



load(file = "data/export_summaries_opr.Rdata")
source(file = "ui-dk.R", encoding = "UTF-8")
outcome_names <- names(export)
max_year <- 2015
ui <- fluidPage(
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
               choices = outcome_names
             ),
             fluidRow(column(
               5,
               selectInput(
                 inputId = "year",
                 label = choose_year,
                 choices = NULL,
                 selected = 2015
               )
               
             ),
             column(
               7,
               selectInput(
                 inputId = "theme",
                 label = choose_theme,
                 choices = theme_names
               )
             )),
             fluidRow(column(
               5,
               radioGroupButtons(
                 inputId = "aggr_level",
                 label = choose_aggr_lv,
                 choices = aggr_choices,
                 justified = TRUE,
                 direction = "vertical"
               )
             )
             ,
             column(
               7,
               radioGroupButtons(
                 inputId = "variable",
                 label = choose_var,
                 choices = variable_choices,
                 justified = TRUE,
                 direction = "vertical",
                 individual = FALSE
               )
             ))
             
           )),
    
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
  
  fluidRow(column(
    12,
    align = "center",
    br(),
    
    # Plots
    fluidRow(
      conditionalPanel("input.aggr_level != 'national'",
                       simpleD3BarOutput("d3_plot_bar")),
      conditionalPanel("input.aggr_level == 'national'",
                       
                       simpleD3LineOutput("d3_plot_line_html")
    )),
    
    br(),
    
    # DataTables
    fluidRow(DTOutput("table_age"))
  ))
  #
  #
  # tabsetPanel(
  #   type = "tabs",
  #   id = "aggr_level",
  #
  #   # TABS ---------------------------------------------------------------------
  #
  #
  #   # Education
  #   tabPanel(ui_edu, value = "edu",
  #            # Row for results
  #            br(),
  #            fluidRow(d3Output("d3_bar_edu")),
  #            br(),
  #
  #            fluidRow(DTOutput("table_edu"))),
  #
  #   # Region
  #   tabPanel(ui_region, value = "region",
  #            br(),
  #
  #            # Row for results
  #            fluidRow(DTOutput("table_region"))),
  #
  #   # National
  #   tabPanel(ui_national, value = "national",
  #            br(),
  #
  #            # Row for results
  #            fluidRow(DTOutput("table_national")))
  #
  # )
  #
  
  
)
