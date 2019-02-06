library(shiny)
library(DT)
library(magrittr)
library(shinyWidgets)
library(data.table)
library(shinyBS)
library(lubridate)
library(markdown)
# devtools::install_github('matthew-phelps/simpled3', force = TRUE)
library(simpled3)

load(file = "data/shiny_dat.rda")
load(file = "data/export_med.Rdata")
source("ui-dk.R", encoding = "UTF-8")
# rmarkdown::render(input = "about-dk.Rmd", output_dir = "html", encoding = "UTF-8")

formatNumbers <- function(dat) {
  x <- copy(dat)
  col_names <- colnames(dat)[-1]
  x[, (col_names) := x[, lapply(.SD, function(i) {
    if (lang == "dk") {
      i[!is.na(i)] <-
        prettyNum(i[!is.na(i)], big.mark = ".", decimal.mark = ",")
    } else if (lang == "en") {
      i[!is.na(i)] <-
        prettyNum(i[!is.na(i)], big.mark = ",", decimal.mark = ".")
    }
    i[is.na(i)] <- "<10"
    i
  }),
  .SDcols = col_names]]

  x[]
}


makeCountDT <- function(dat, group_var){
  DT::datatable(
  data = dat,
  extensions = 'Buttons',
  rownames = FALSE,
  class = ' hover row-border',
  options = list(
    columnDefs = list(list(
      # Hides the "flag" column
      visible = FALSE, targets = 0
    )),
    buttons = list('csv'),
    initComplete = JS(
      # Table hearder background color
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#e7e7e7'});",
      "}"
    )
  )
) %>%
  formatStyle('Total',  fontWeight = 'bold') %>%
  formatStyle(group_var,  backgroundColor = "#e7e7e7") %>%
  formatStyle("flag",
              target = "row",
              fontWeight = styleEqual(c(0, 1), c("normal", "bold")))
}



makeRateDT <- function(dat, group_var){ 
  DT::datatable(
  data = dat,
  extensions = 'Buttons',
  rownames = FALSE,
  class = 'hover row-border',
  options = list(
    buttons = list('csv'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#e7e7e7'});",
      "}"
    )
  )
) %>%
  formatStyle(group_var,  backgroundColor = "#e7e7e7")
}