# LIBRARIES ----------------------------------------------------
# devtools::install_github("rstudio/profvis")
# library(profvis)
# library(reactlog)
# options(shiny.reactlog = TRUE)
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
enableBookmarking("url")
# LANGUAGE UI ---------------------------------------------------------

print(Sys.getlocale())
lang = "dk"
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
  .SDcols = col_2names]]
  
  x[]
}




# DATATABLE FUNCTIONS ----------------------------------------------------

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


makeCountDT <- function(dat, group_var, thousands_sep, dt_title, messageBottom, n_col) {
  col_format <- c(ui_sex_levels, "Total")
  DT::datatable(
    data = dat,
    extensions = 'Buttons',
    rownames = FALSE,
    class = ' hover row-border',
    options = list(
      language = list(url = "Danish.json"),
      ordering = FALSE,
      dom = "tB",
      columnDefs = list(list(render = formatSuppressedValues, targets = "_all")),
      buttons = list(
        list(extend = "pdf",
             messageTop = dt_title,
             messageBottom = messageBottom),
        list(extend = "excel",
             messageTop = dt_title,
             messageBottom = messageBottom)
      ),
      initComplete = header_JS
    )
  ) %>%
    formatStyle(1:n_col, borderColor = "white") %>% 
    formatStyle('Total',  fontWeight = 'bold') %>%
    formatStyle(group_var,  backgroundColor = DT_background_color, color = "white") %>%
    formatStyle(
      # Bolds the "Totals" row which has character == "Total" in column 1
      1,
      target = "row",
      fontWeight = styleEqual(
        levels = c("Total"),
        values =  c("bold"),
        default = "normal"
      )
    )
}


makeCountKomDT <-
  function(dat, group_var, thousands_sep, dt_title, messageBottom, n_col) {
    col_format <- c(ui_sex_levels, "Total")
    DT::datatable(
      data = dat,
      extensions = 'Buttons',
      rownames = FALSE,
      class = ' hover row-border',
      options = list(
        language = list(url = "Danish.json"),
        ordering = TRUE,
        lengthMenu = list(c(15, 50, -1), c('15', '50', 'Alle')),
        pageLength = 15,
        dom = "lftBsp",
        columnDefs = list(list(render = formatSuppressedValues, targets = "_all")),
        buttons = list(
          list(extend = "pdf",
               messageTop = dt_title,
               messageBottom = messageBottom),
          list(extend = "excel",
               messageTop = dt_title,
               messageBottom = messageBottom)
        ),
        initComplete = header_JS
      )
    ) %>%
      formatStyle(1:n_col, borderColor = "white") %>% 
      formatStyle('Total',  fontWeight = 'bold') %>%
      formatStyle(group_var,  backgroundColor = DT_background_color, color = "white") %>%
      formatStyle(
        # Bolds the "Totals" row which has character == "Total" in column 1
        1,
        target = "row",
        fontWeight = styleEqual(
          levels = c("Total"),
          values =  c("bold"),
          default = "normal"
        )
      )
    
  }


makeRateDT <-
  function(dat,
           group_var,
           dt_title,
           messageBottom, n_col) {
    col_format <- c(ui_sex_levels)
    DT::datatable(
      data = dat,
      extensions = 'Buttons',
      rownames = FALSE,
      class = 'hover row-border',
      options = list(
        language = list(url = "Danish.json"),
        ordering = FALSE,
        dom = "tB",
        columnDefs = list(list(render = formatNAValues, targets = "_all")),
        buttons = list(
          list(extend = "pdf",
               messageTop = dt_title,
               messageBottom = messageBottom),
          list(extend = "excel",
               messageTop = dt_title,
               messageBottom = messageBottom)
        ),
        initComplete = header_JS
      )
    )  %>%
      formatStyle(1:n_col, borderColor = "white") %>% 
      formatStyle(group_var,  backgroundColor = DT_background_color, color = "white")
  }

makeRateKomDT <-
  function(dat,
           group_var,
           dt_title,
           messageBottom = messageBottom, n_col) {
    col_format <- c(ui_sex_levels)
    DT::datatable(
      data = dat,
      extensions = 'Buttons',
      rownames = FALSE,
      class = 'hover row-border',
      options = list(
        language = list(url = "Danish.json"),
        lengthMenu = list(c(15, 50, -1), c('15', '50', 'Alle')),
        pageLength = 15,
        dom = "lftBp",
        columnDefs = list(list(render = formatNAValues, targets = "_all")),
        buttons = list(
          list(extend = "pdf",
               messageTop = dt_title,
               messageBottom = messageBottom),
          list(extend = "excel",
               messageTop = dt_title,
               messageBottom = messageBottom)
        ),
        initComplete = header_JS
      )
    ) %>%
      formatStyle(1:n_col, borderColor = "white") %>% 
      formatStyle(group_var,  backgroundColor = DT_background_color, color = "white")
  }


# LEAFLET MAPS -------------------------------------------------------


makeMapPopup <- function(geo_name, var_title1, data) {
  # geo_name is place name. var_title1 and var_title two is the title broken
  # where two spaces occur ("  "). Data is the datapoint passed to the function
  out <- paste0(
    "<strong><center>",
    geo_name,
    '</strong></center>',
    '<p style = "font-size:0.8em; margin-bottom:0px">',
    var_title1, ": ",
    '</p>',
    '<strong><center><p style = "font-size:1.2em; margin-bottom:0px">',
    formatC(data, ),
    "</strong></p></center>"
  )
  lapply(out, htmltools::HTML)
}

makeLeaflet <-
  function(map_data,
           fill_colors,
           label_popup,
           mini_map_lines,
           element_id) {
    leaflet(elementId = element_id,
            options = leafletOptions(minZoom = 7,
                                     preferCanvas = TRUE)) %>%
      setView(lng = 10.408,
              lat = 56.199752,
              zoom = 7,) %>%
      setMaxBounds(
        lng1 = 8.1,
        lng2 = 12.7,
        lat1 = 54.5,
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
          offset = c(0, 0),
          sticky = FALSE,
          textsize = "17px",
          direction = "auto",
          opacity = 1
        ),
        highlightOptions = highlightOptions(weight = 4,
                                            bringToFront = TRUE)
      ) %>%
      addPolylines(
        data = mini_map_lines,
        lng = ~ X1,
        lat = ~ X2,
        color = "grey",
        weight = 4
      )
  }


# ABOUT PANEL ------------------------------------------------------------

makeAboutTables <-
  function(dat,
           col_names,
           order = FALSE,
           paging = FALSE,
           dom = "Bt",
           search = FALSE) {
    colnames(dat) <- col_names
    DT::datatable(
      data = dat,
      extensions = 'Buttons',
      rownames = FALSE,
      class = ' hover row-border',
      selection = "multiple",
      options = list(
        ordering = order,
        paging = paging,
        searching = search,
        pageLength = 20,
        dom = dom,
        buttons = list('pdf'),
        initComplete = header_JS,
        autoWidth = TRUE
        # columnDefs = list(list(targets = c(0), visible = TRUE, width= '35%'))
      )
    )
  }







# CHD PANEL ---------------------------------------------------------------

shiny_dat_chd <- readRDS("data/chd/shiny_dat_chd.rds")
year_max_chd <- 2017
year_min_chd <- 2014
year_choices_chd <- year_min_chd:year_max_chd


makeRateDT_chd <-
  function(dat,
           group_var,
           dt_title,
           messageBottom,
           n_col,
           digits) {
    col_format <- c(ui_sex_levels)
    DT::datatable(
      data = dat,
      extensions = 'Buttons',
      rownames = FALSE,
      class = 'hover row-border',
      options = list(
        language = list(url = "Danish.json"),
        ordering = FALSE,
        dom = "tB",
        columnDefs = list(list(render = formatNAValues, targets = "_all")),
        buttons = list(
          list(extend = "pdf",
               messageTop = dt_title,
               messageBottom = messageBottom),
          list(extend = "excel",
               messageTop = dt_title,
               messageBottom = messageBottom)
        ),
        initComplete = header_JS
      )
    )  %>%
      formatStyle(1:n_col, borderColor = "white") %>% 
      formatStyle(group_var,  backgroundColor = DT_background_color, color = "white") %>% 
      formatCurrency(columns = 2:n_col, currency = "", interval = 3, mark = thousands_sep, dec.mark = dec_mark, digits = digits)
  }
