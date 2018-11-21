library(shiny)
library(shinyWidgets)
library(shinyjs)
library(data.table)
library(ggplot2)
library(r2d3)
library(shinyBS)
library(lubridate)
library(DT)




load(file = "data/export_summaries_opr.Rdata")
source(file = "ui-dk.R", encoding = "UTF-8")
outcome_names <- names(export)
max_year <- 2015
ui <- fluidPage(
  titlePanel("HjerteTal2"),
  fluidRow(column(
    4,
    wellPanel(
      selectInput(
        inputId = "outcome",
        label = choose_outcome,
        choices = outcome_names
      ),
      selectInput(
        inputId = "theme",
        label = choose_theme,
        choices = theme_names
      ),
      
      
      selectInput(
        inputId = "year",
        label = choose_year,
        choices = c(2006:max_year),
        selected = 2015
      ),
        
        radioGroupButtons(
          inputId = "aggr_level",
          label = choose_aggr_lv,
          choices = aggr_choices
          )
      
        
      
    )
  ),
  
  column(
    7,
    h2(textOutput("outcome_title")),
    hr(),
    textOutput("outcome_description")
    
  )),
  
  fluidRow(
    column(10, align = "center",
    br(),
    fluidRow(d3Output("d3_bar_age")),
    br(),
    
    fluidRow(DTOutput("table_age"))
    )
  )
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
