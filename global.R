# LIBRARIES ----------------------------------------------------
# devtools::install_github("rstudio/profvis")
# library(profvis)
# remotes::install_github("rstudio/reactlog")
library(reactlog)
reactlog_enable()
# devtools::install_version("shiny", version = "1.5.0")
suppressPackageStartupMessages({
library(shiny)
library(DT)
library(shinyWidgets)
library(data.table)
library(shinyjs) # Hides un-used tabs
library(classInt) # For choropleth map breaks
library(leaflet)
library(manipulateWidget)
# library(shinycssloaders)
# devtools::install_github('matthew-phelps/simpled3', force = TRUE)
library(simpled3)
library(mapview) # For map downloads
# devtools::install_github("ropensci/plotly")
library(plotly)
library(fst)
library(magrittr)
})
print(suppressMessages(webshot:::find_phantom()))
if (is.null(suppressMessages(webshot:::find_phantom()))) {
  webshot::install_phantomjs()
}

# install.packages(c("V8","jqr","protolite","crul","jsonvalidate","httpcode","urltools","maptools"))
# install.packages(c("geojson","geo2jsonino","geojsonlint","rgeos","rmapshaper"))
print(suppressMessages(webshot:::find_phantom()))



# LOAD R CODE -------------------------------------------------------------
# This should be done automatically in shiny versions >= 1.5
# However, on deployment to shinyapps, it seems these codes are note loaded
files <- list.files(path = "r/", full.names = TRUE)
sapply(files, source)

enableBookmarking("url")
# LANGUAGE UI ---------------------------------------------------------
lang = "en"
if (lang == "dk") {
  thousands_sep <- "."
  dec_mark <- ","
} else {
  thousands_sep <- ","
  dec_mark <- "."
}

# OBJECTS ------------------------------------------------------------
year_max <- 2017

data_path <- file.path(paste0("data/shiny_dat_", lang, ".rds"))
shiny_dat <- readRDS(file = data_path)
dk_sp <- readRDS(file = "data/dk_sp_data.rds")
pop <- fread("data/pop_summary_age.txt")

source("ui/ui-common.R")
ui_file_path <- file.path(paste0("ui/ui-", lang, ".R"))
source(ui_file_path, encoding = "UTF-8")

valid_output_combos <- readRDS(file = "data/valid_output_combos.rds")
variables_not_used <- variable_ui[grepl("count_n_ambulatory|count_n_bed", code_name), shiny_code]

male_color <- "#10c8a6"
female_color <- "#166abd"

male_color <- "#19b9b1"
female_color <- "#ea8438"


# COLORS ------------------------------------------------------------------
hfBlue <- "#002A3A"
graph_colors <- c(male_color, female_color)
rm(male_color, female_color)

single_val_col <- hfBlue

DT_background_color <- "#002A3A"
DT_background_color <- "#193f4d"


# DATATABLE JS FUNCTIONS----------------------------------------------------
# From:
# https://stackoverflow.com/questions/46694351/r-shiny-datatables-replace-numeric-with-string-and-sort-as-being-less-than-numer
formatSuppressedValues <- JS(
  "
  function(data, type) {

    if (type !== 'display') return data;
    if (data !== '0') return data;
    return '<4';
  }
"
)


formatNAValues <- JS(
  "
  function(data, type) {

    if (type !== 'display') return data;
    if (data !== '0,0') return data;
    return 'NA';
  }
"
)

header_JS <- JS(
  # Table hearder background color
  paste0("function(settings, json) {",
  "$(this.api().table().header()).css({'background-color': '", DT_background_color, "', 'color':'white'});",
  "}"
  )
)




# CHD PANEL ---------------------------------------------------------------
shiny_dat_chd <- readRDS("data/chd/shiny_dat_chd.rds")
year_max_chd <- 2017
year_min_chd <- 2014
year_choices_chd <- year_min_chd:year_max_chd
