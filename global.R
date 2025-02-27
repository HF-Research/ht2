# LIBRARIES ----------------------------------------------------
# devtools::install_github("rstudio/profvis")
# library(profvis) # used to check performance, almost certainly not needed unless a major change is done to the shiny code
# remotes::install_github("rstudio/reactlog")
# library(reactlog)

options("shiny.reactlog" = TRUE)
# devtools::install_version("shiny", version = "1.5.0")
suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(shinyWidgets)
  library(data.table)
  library(tidyverse)
  library(shinyjs) # Hides un-used tabs
  library(manipulateWidget)
  library(sf)
  # devtools::install_github("ropensci/plotly")
  library(plotly)
  library(leaflet)
  library(fst)
  
  library(magrittr)
})


# LOAD R CODE ------------------------------------------------------------- This
# should be done automatically in shiny versions >= 1.5 However, on deployment
# to shinyapps, it seems these codes are note loaded automatically, so I do it
# explicitly here
files <- list.files(path = "r/", full.names = TRUE)
nix <- sapply(files, source)

enableBookmarking("url")
# LANGUAGE UI ---------------------------------------------------------
lang = "dk"
if (lang == "dk") {
  thousands_sep <- "."
  dec_mark <- ","
} else {
  thousands_sep <- ","
  dec_mark <- "."
}


# OBJECTS ------------------------------------------------------------

data_path <- file.path(paste0("data/shiny_dat_", lang, ".rds"))
shiny_dat <- readRDS(file = data_path)
dk_sf <- readRDS(file = "data/dk_sf_data.rds")
pop <- fread("data/pop_summary_age.txt")


year_max <- shiny_dat$d1$national$v1[, max(year)]

source("ui/ui-common.R")
ui_file_path <- file.path(paste0("ui/ui-", lang, ".R"))
source(ui_file_path, encoding = 'UTF-8')

valid_output_combos <-
  readRDS(file = "data/valid_output_combos.rds")
variables_not_used <-
  variable_ui[grepl("n_ambulatory|n_bed", code_name), shiny_code]


# COLORS ------------------------------------------------------------------

hfBlue <- "#400000"

male_color <- "#4058f4"
female_color <- "#CFAAF6"
graph_colors <- c(male_color, female_color)
rm(male_color, female_color)

single_val_col <- hfBlue

DT_background_color <- "#400000"
DT_background_color <- "#400000"
DT_background_color <- "white"
DT_text_color <- hfBlue

# PLOTLY PARAMETERS ----------------------------------------------------------
axis_font_size <- 20
legend_font_size <- 17
tick_font_size <- 15

# CHD PANEL ---------------------------------------------------------------
shiny_dat_chd <- readRDS("data/chd/shiny_dat_chd.rds")


# STARTING CONDITIONS -----------------------------------------------------
starting_vals_var <- validate_selected_vars(
  aggr_selected = "national",
  outcome_code = "d1",
  variables_not_used = variables_not_used,
  lang = lang,
  selected_var = "v1"
)
starting_vals_agg <- make_agg_choices(
  var_selected = "v1",
  outcome_code = "d1",
  valid_output_combos = valid_output_combos,
  aggr_choices = aggr_choices,
  input_aggr_level = "national"
)

