##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param map_data
##' @param fill_colors
##' @param label_popup
##' @param mini_map_lines
##' @param element_id
makeLeaflet <- function(map_data, fill_colors, label_popup, mini_map_lines,
                        element_id) {
  # Ensure label_popup is formatted to avoid scientific notation
  label_popup <- sapply(label_popup, function(x) {
    if (is.numeric(x)) {
      format(x, scientific = FALSE)
    } else {
      x
    }
  })
  
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
      fillOpacity = 0.9,
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
