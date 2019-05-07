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
library(mapview)
if (is.null(suppressMessages(webshot:::find_phantom()))) {
  webshot::install_phantomjs()
}

# This is a fork, because of the issue highlighted here with the master branch:
# https://github.com/rstudio/leaflet/issues/347
# devtools::install_github('matthew-phelps/leaflet.minicharts')
library(leaflet.minicharts)

# LANGUAGE UI ---------------------------------------------------------
lang = "dk"
if (lang == "dk") {
  thousands_sep <- "."
  dec_mark <- ","
} else {
  thousands_sep <- ","
  dec_mark <- "."
}
2
# OBJECTS ------------------------------------------------------------
data_path <- file.path(paste0("data/shiny_dat_", lang, ".rds"))
shiny_dat <- readRDS(file = data_path)
edu <- fread(file = "data/edu_description.csv")
dk_sp <- readRDS(file = "data/dk_sp_data.rds")
year_max <- 2016

ui_file_path <- file.path(paste0("ui/ui-", lang, ".R"))
source(ui_file_path, encoding = "UTF-8")

valid_output_combos <- fread("data/valid_output_combos.txt")
valid_output_combos[, var := paste0("count_", var)]
variables_not_used <- c("count_n_ambulatory", "count_n_bed_days")


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

# DATA TABLE FUNCTIONS ----------------------------------------------------

# From:
# https://stackoverflow.com/questions/46694351/r-shiny-datatables-replace-numeric-with-string-and-sort-as-being-less-than-numer
formatSuppressedValues <- JS(
  "
  function(data, type) {

    if (type !== 'display') return data;
    if (data !== 0) return data;
    return '<10';
  }
"
)

makeCountDT <- function(dat, group_var, thousands_sep) {
  col_format <- c(ui_sex_levels, "Total")
  DT::datatable(
    data = dat,
    extensions = 'Buttons',
    rownames = FALSE,
    class = ' hover row-border',
    options = list(
      dom = "tB",
      columnDefs = list(
        list(# Hides the "flag" column
          visible = FALSE, targets = 0),
        list(render = formatSuppressedValues, targets = "_all")
      ),
      buttons = list('pdf', 'excel'),
      initComplete = JS(
        # Table hearder background color
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#e7e7e7'});",
        "}"
      )
    )
  ) %>%
    # formatCurrency(col_format,
    #                currency = "",
    #                interval = 3,
    #                mark = thousands_sep,
    # digits = 0) %>%
    formatStyle('Total',  fontWeight = 'bold') %>%
    formatStyle(group_var,  backgroundColor = "#e7e7e7") %>%
    formatStyle("flag",
                target = "row",
                fontWeight = styleEqual(c(0, 1), c("normal", "bold")))
}

makeRateDT <-
  function(dat,
           group_var,
           thousands_sep,
           digits,
           dec_mark) {
    col_format <- c(ui_sex_levels)
    DT::datatable(
      data = dat,
      extensions = 'Buttons',
      rownames = FALSE,
      class = 'hover row-border',
      options = list(
        dom = "tB",
        buttons = list('pdf', 'excel'),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#e7e7e7'});",
          "}"
        )
      )
    ) %>%
      formatCurrency(
        col_format,
        currency = "",
        interval = 3,
        mark = thousands_sep,
        digits = digits,
        dec.mark = dec_mark
      ) %>%
      formatStyle(group_var,  backgroundColor = "#e7e7e7")
  }


# LEAFLET MAPS -------------------------------------------------------
pal <- colorBin("YlOrRd", NULL, bins = 5, reverse = FALSE)
makeLeaflet <-
  function(map_data,
           fill_colors,
           label_popup,
           mini_map_lines) {
    leaflet(options = leafletOptions(minZoom = 7,
                                     preferCanvas = TRUE)) %>%
      setView(lng = 10.408,
              lat = 56.199752,
              zoom = 7, ) %>%
      setMaxBounds(
        lng1 = 7.7,
        lat1 = 54.5,
        lng2 = 13.3,
        lat2 = 58.0
      ) %>%
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
          opacity = 1
        )
      ) %>%
      addPolylines(
        data = mini_map_lines,
        lng = ~ X1,
        lat = ~ X2,
        color = "grey",
        weight = 5
      )
  }

# ABOUT PANEL ------------------------------------------------------------
col_subset <-
  c(paste0("name_", lang),
    paste0("desc_", lang),
    "code_simple",
    "diag_type",
    "pat_type")
diag <- about_dat_diag[, ..col_subset]
colnames(diag) <- col_names_diag
diag_DT <- DT::datatable(
  data = diag,
  extensions = 'Buttons',
  rownames = FALSE,
  class = 'hover row-border',
  options = list(
    paging = FALSE,
    searching = FALSE,
    pageLength = 13,
    dom = "Bt",
    buttons = list('pdf'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#e7e7e7'});",
      "}"
    )
  )
)

col_subset <-
  c(paste0("name_", lang),
    paste0("desc_", lang),
    "code_simple")
opr <- about_dat_opr[, ..col_subset]
colnames(opr) <- col_names_opr
opr_DT <- DT::datatable(
  data = opr,
  extensions = 'Buttons',
  rownames = FALSE,
  class = 'hover row-border',
  options = list(
    dom = "Bt",
    paging = FALSE,
    searching = FALSE,
    pageLength = 13,
    buttons = list('pdf'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#e7e7e7'});",
      "}"
    )
  )
)

col_subset <-
  c(paste0("name_", lang),
    paste0("desc_", lang),
    "code_simple"
    )
med <- about_dat_med[, ..col_subset]
colnames(med) <- col_names_med
med_DT <- DT::datatable(
  data = med,
  extensions = 'Buttons',
  rownames = FALSE,
  class = 'hover row-border',
  options = list(
    dom = "Bt",
    paging = FALSE,
    searching = FALSE,
    pageLength = 13,
    buttons = list('pdf'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#e7e7e7'});",
      "}"
    )
  )
)

col_subset <-
  c(paste0("edu_name_", lang),
    paste0("long_desc_", lang),
    "code_simple")
edu <- edu[, ..col_subset]
colnames(edu) <- col_names_edu
edu_DT <- DT::datatable(
  data = edu,
  extensions = 'Buttons',
  rownames = FALSE,
  class = 'hover row-border',
  options = list(
    dom = "Bt",
    buttons = list('pdf'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#e7e7e7'});",
      "}"
    )
  )
)
