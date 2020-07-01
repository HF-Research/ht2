##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param year_val
##' @param is_geo
##' @param is_5year_mortality
##' @param
make_year_choices <- function(year_val = year_val,
                              is_geo = isGeo(),
                              is_5year_mortality = is5YearMortality()) {
  if (year_val != "") {
    selected_year <- year_val
    # Set year-range to be used by udateSelectInput()
    if (is_geo &&
        !is_5year_mortality) {
      year_range <- c(2009:year_max)
      if (year_val < 2009)
        selected_year <- 2009
      
    } else if (is_geo &&
               is_5year_mortality) {
      year_range <- c(2009:(year_max - 4))
      if (year_val < 2009) {
        selected_year <- 2009
      } else if (year_val > (year_max - 4)) {
        selected_year <- year_max - 4
      }
      
    } else if (!is_geo &&
               is_5year_mortality) {
      year_range <- c(2006:(year_max - 4))
      if (year_val > (year_max - 4)) {
        selected_year <- year_max - 4
      }
      
    } else {
      year_range <- c(2006:year_max)
      
    }
    return(list(selected_year = selected_year,
                year_range = year_range))
    
  } else {
    return(list(
      selected_year = year_max,
      year_range = 2006:year_max
    ))
    
  }
}
