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
    legend_opacity = 0.9
    name_lang <- paste0("name_", lang)
    
    fill_data <-
      subset_vars[get(ui_year) >= 2009] %>% 
    .[get(var_name) == 0, (var_name) := NA]
    
    fill_data_male <- fill_data[get(ui_sex) == "male"][[var_name]]
    fill_data_female <- fill_data[get(ui_sex) == "female"][[var_name]]
    
    
    legend_title <- stringr::str_wrap(var_name, 25) %>% 
      gsub("\\n", "<br>", .)
    popup_var_title_1 <- gsub("  .*", "", var_name)
    
    # Male map
    map_data <-  map_data_main$male
    pal <- 
      colorNumeric(palette = "YlOrRd", domain = fill_data_male,)
    
    # This is created so NA doesn't appear on the legend
    pal_NA <- colorNumeric("YlOrRd", fill_data_male, na.color=rgb(0,0,0,0))
    
    fill_colors <-
      ~ pal(map_data[[var_name]])
    popup <-
      makeMapPopup(geo_name = map_data[[pretty_aggr_level]],
                   var_title1 = popup_var_title_1,
                   map_data[[var_name]])
    
    map_m <- makeLeaflet(
      map_data = map_data,
      fill_colors = fill_colors,
      label_popup = popup,
      mini_map_lines = dk_sp$mini_map_lines,
      element_id = "map_male"
    ) %>% 
      addLegend(
        "topright",
        pal = pal_NA,
        values = fill_data_male,
        title = legend_title,
        bins = 4,
        na.label = "",
        layerId = "legend",
        opacity = legend_opacity
      )
    
    
    # Female map
    map_data <-  map_data_main$female
    pal <- 
      colorNumeric(palette = "YlOrRd", domain = fill_data_female)
    
    pal_NA <- colorNumeric("YlOrRd", fill_data_female, na.color=rgb(0,0,0,0))
    fill_colors <-
      ~ pal(map_data[[var_name]])
    popup <-
      makeMapPopup(geo_name = map_data[[pretty_aggr_level]],
                   var_title1 = popup_var_title_1,
                   map_data[[var_name]])
    map_f <- makeLeaflet(
      map_data = map_data,
      fill_colors = fill_colors,
      label_popup = popup,
      mini_map_lines = dk_sp$mini_map_lines,
      element_id = "map_female"
    ) %>%
      addLegend(
        "topright",
        pal = pal_NA,
        values = fill_data_female,
        title = legend_title,
        bins = 4,
        na.label = "",
        layerId = "legend",
        opacity = legend_opacity
     )
    
    # css_fix <- "div.info.legend.leaflet-control br {clear: both;}"  # CSS to correct spacing
    # html_fix <- htmltools::tags$style(type = "text/css", css_fix)   # Convert CSS to HTML
    # map_f %<>% htmlwidgets::prependContent(html_fix)                # Insert into leaflet HTML code
    # map_m %<>% htmlwidgets::prependContent(html_fix)                # Insert into leaflet HTML code
    return(list(map_m = map_m,
                map_f = map_f,
                fill_data_male = fill_data_male,
                fill_data_female = fill_data_female
                ))
  }
