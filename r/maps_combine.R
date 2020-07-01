##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param var_name
##' @param subset_vars
##' @param select_percent_rates
##' @param thousands_sep
##' @param dec_mark
##' @param map_data_main
##' @param dk_sp
##' @param pretty_aggr_level
maps_combine <-
  function(var_name = prettyVariable()[2],
           lang = lang,
           subset_vars = subsetVars(),
           ui_year = ui_year,
           select_percent_rates = selectPercentOrRate(),
           thousands_sep = thousands_sep,
           dec_mark = dec_mark,
           map_data_main = mapData(),
           dk_sp = dk_sp,
           pretty_aggr_level = prettyAggr_level()) {
    
    name_lang <- paste0("name_", lang)
    
    fill_data <-
      subset_vars[get(ui_year) >= 2009,][[var_name]]
    
    fill_data[fill_data == 0] <- NA
    # Define breaks using the "pretty" algorithm
    map_breaks <-
      suppressWarnings(classIntervals(fill_data, style = "pretty", n = 5))
    pal <-
      colorBin(
        palette = "YlOrRd",
        bins = length(map_breaks$brks),
        domain = map_breaks$brks
      )
    
    if (select_percent_rates) {
      # If variable is percent:
      labFormatter <- function(type, cuts) {
        n = length(cuts)
        cuts <- round(cuts, digits = 1)
        paste0(cuts[-n], "% &ndash; ", cuts[-1], "%")
      }
    } else {
      # If variable is rate:
      labFormatter <- function(type, cuts) {
        n = length(cuts)
        cuts_formatted <- formatC(
          round(cuts),
          digits = 0,
          format = "d",
          big.mark = thousands_sep,
          decimal.mark = dec_mark
        )
        paste0(cuts_formatted[-n], " &ndash; ", cuts_formatted[-1])
      }
    }
    
    
    legend_title <- gsub("  ", "<br>", var_name)
    popup_var_title_1 <- gsub("  .*", "", var_name)
    
    
    # Male map
    map_data <-  map_data_main$male
    fill_colors <-
      ~ pal(map_data@data[[var_name]])
    
    
    popup <-
      makeMapPopup(geo_name = map_data@data[[pretty_aggr_level]],
                   var_title1 = popup_var_title_1,
                   map_data@data[[var_name]])
    
    map_m <- makeLeaflet(
      map_data = map_data,
      fill_colors = fill_colors,
      label_popup = popup,
      mini_map_lines = dk_sp$mini_map_lines,
      element_id = "map_male"
    )
    
    # Female map
    map_data <-  map_data_main$female
    
    fill_colors <-
      ~ pal(map_data@data[[var_name]])
    popup <-
      makeMapPopup(geo_name = map_data@data[[pretty_aggr_level]],
                   var_title1 = popup_var_title_1,
                   map_data@data[[var_name]])
    
    
    map_f <- makeLeaflet(
      map_data = map_data,
      fill_colors = fill_colors,
      label_popup = popup,
      mini_map_lines = dk_sp$mini_map_lines,
      element_id = "map_female"
    ) %>%
      addLegend(
        "topright",
        pal = pal,
        values = fill_data,
        title = legend_title,
        labels = legend_labels,
        layerId = "legend",
        labFormat = function(type, cuts, p = NULL) {
          type <- type
          cuts <- cuts
          labFormatter(type = type,
                       cuts = cuts)
        }
      )
    
    map_m_legend <- map_m %>% 
      addLegend(
        "topright",
        pal = pal,
        values = fill_data,
        title = legend_title,
        labels = legend_labels,
        layerId = "legend",
        labFormat = function(type, cuts, p = NULL) {
          type <- type
          cuts <- cuts
          labFormatter(type = type,
                       cuts = cuts)
        }
      )
    return(list(map_m = map_m,
                map_f = map_f,
                map_m_legend = map_m_legend))
  }
