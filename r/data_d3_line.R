##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param dat
##' @param pretty_var_singular
data_d3_line <- function(dat = dtCast(), pretty_var_singular =
                         prettyVariableSingular(),
                         ui_year = ui_year) {

  vars <-
    c("group_var",
      grep(
        pretty_var_singular,
        colnames(dat),
        fixed = TRUE,
        # because special characters exits
        value = TRUE
      ))
  dat <- dat[, ..vars]
  setnames(dat, c(ui_year, "male", "female")) # TODO: needs to be language agnostic
  
  # Column containing variable name to send to D3. TODO: send this data as
  # single data point - so change d3 widget
  dat[, variable := pretty_var_singular]
  
}
