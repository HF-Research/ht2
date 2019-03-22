library(shiny)
library(DT)
library(shinyWidgets)
library(data.table)
library(shinyBS)
library(lubridate)
library(shinyjs)
# devtools::install_github('matthew-phelps/simpled3', force = TRUE)
library(simpled3)
shiny_dat <- readRDS(file = "data/shiny_dat.rds")

# LANGUAGE ----------------------------------------------------------------
lang = "dk"

# SOURCE ----------------------------------------------------------------
ui_file_path <- file.path(paste0("ui/ui-", lang, ".R"))
source(ui_file_path, encoding = "UTF-8")
source("r/PrepDefinitions.R")
year_max <- 2016

# MAIN PANNEL -------------------------------------
formatNumbers <- function(dat, lang) {
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
    buttons = list(list(
      extend = "collection",
      buttons = c("excel", "pdf"),
      exportOptions = list(columns = ":visible"),
      text = "Hente"
    )),
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


# ABOUT tabPanel-------------------------------------------------------------------

col_subset <- c(paste0("name_", lang), "icd_simple", "ambulant", "diag_type", "pat_type")
diag <- diag[, ..col_subset]
colnames(diag) <- col_names_diag
diag_DT <- DT::datatable(
  data = diag,
  extensions = 'Buttons',
  rownames = FALSE,
  class = 'hover row-border',
  options = list(
    buttons = list('pdf'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#e7e7e7'});",
      "}"
    )
  )
)

col_subset <- c(paste0("name_", lang), "icd_simple", "grep_strings")
opr <- opr[, ..col_subset]
colnames(opr) <- col_names_opr
opr_DT <- DT::datatable(
  data = opr,
  extensions = 'Buttons',
  rownames = FALSE,
  class = 'hover row-border',
  options = list(
    buttons = list('pdf'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#e7e7e7'});",
      "}"
    )
  )
)

col_subset <- c(paste0("name_", lang), "atc_simple", "grep_strings")
med <- med[, ..col_subset]
colnames(med) <- col_names_med
med_DT <- DT::datatable(
  data = med,
  extensions = 'Buttons',
  rownames = FALSE,
  class = 'hover row-border',
  options = list(
    buttons = list('pdf'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#e7e7e7'});",
      "}"
    )
  )
)

col_subset <- c(paste0("long_desc_", lang), "code_simple")
edu <- code_tables$edu[, ..col_subset]
colnames(edu) <- col_names_edu
edu_DT <- DT::datatable(
  data = edu,
  extensions = 'Buttons',
  rownames = FALSE,
  class = 'hover row-border',
  options = list(
    buttons = list('pdf'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#e7e7e7'});",
      "}"
    )
  )
)

