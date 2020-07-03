##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param geo_name
##' @param var_title1
##' @param data
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
