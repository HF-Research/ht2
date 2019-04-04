# LIBRARIES ----------------------------------------------------
# devtools::install_github("rstudio/profvis")
library(profvis)
library(shiny)
library(DT)
library(shinyWidgets)
library(data.table)
library(shinyBS)
library(shinyjs)
library(leaflet)
library(manipulateWidget)
library(shinycssloaders)
# devtools::install_github('matthew-phelps/simpled3', force = TRUE)
library(simpled3)


# This is a fork, because of the issue highlighted here with the master branch:
# https://github.com/rstudio/leaflet/issues/347
# devtools::install_github('matthew-phelps/leaflet.minicharts')
# library(leaflet.minicharts)


# OBJECTS ------------------------------------------------------------

shiny_dat <- readRDS(file = "data/shiny_dat.rds")
dk_sp <- readRDS(file = "data/dk_sp_data.rds")
diag <- fread("data/definitions_diag.csv", encoding = "UTF-8")
opr <- fread("data/definitions_opr.csv", encoding = "UTF-8")
med <- fread("data/definitions_med.csv", encoding = "UTF-8")
pop_summary_weighted <- fread("data/pop_summary_weighted.csv")
setkey(pop_summary_weighted, year, sex, grouping)

setkey(pop_ref, sex, age_group)
# LANGUAGE UI ---------------------------------------------------------
lang = "dk"
if (lang == "dk") {
  thousands_sep <- "."
  dec_mark <- ","
} else {
  thousands_sep <- ","
  dec_mark <- "."
}

ui_file_path <- file.path(paste0("ui/ui-", lang, ".R"))
source(ui_file_path, encoding = "UTF-8")

year_max <- 2016

# FUNCTIONS ------------------------------------------------
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

makeCountDT <- function(dat, group_var, thousands_sep) {
  col_format <- c(ui_sex_levels, "Total")
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
      buttons = list(
        list(
          extend = "collection",
          buttons = c("excel", "pdf"),
          exportOptions = list(columns = ":visible"),
          text = "Hente"
        )
      ),
      initComplete = JS(
        # Table hearder background color
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#e7e7e7'});",
        "}"
      )
    )
  ) %>%
    formatCurrency(col_format,
                   currency = "",
                   interval = 3,
                   mark = thousands_sep,
                   digits = 0) %>%
    formatStyle('Total',  fontWeight = 'bold') %>%
    formatStyle(group_var,  backgroundColor = "#e7e7e7") %>%
    formatStyle("flag",
                target = "row",
                fontWeight = styleEqual(c(0, 1), c("normal", "bold")))
}

makeRateDT <- function(dat, group_var, thousands_sep, digits, dec_mark) {
  col_format <- c(ui_sex_levels)
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
    formatCurrency(col_format,
                   currency = "",
                   interval = 3,
                   mark = thousands_sep,
                   digits = digits,
                   dec.mark = dec_mark) %>%
    formatStyle(group_var,  backgroundColor = "#e7e7e7")
}


# ABOUT PANEL ------------------------------------------------------------
col_subset <-
  c(paste0("name_", lang),
    "icd_simple",
    "ambulant",
    "diag_type",
    "pat_type")
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


# LEAFLET MAPS -------------------------------------------------------
pal <- colorBin("YlOrRd", NULL, bins = 5, reverse = FALSE)
makeLeaflet <- function(map_data, fill_colors, label_popup){
  leaflet() %>%
  setView(lng = 10.3018,
          lat = 56.179752,
          zoom = 7) %>%
  addPolygons(
    data = map_data,
    fillColor  = fill_colors,
    weight = 1,
    opacity = 1,
    color = "grey",
    fillOpacity = 0.7,
    label = label_popup,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", # CSS styles
                   padding = "3px 8px"),
      textsize = "17px",
      direction = "auto",
      opacity = 1)
  )
}
