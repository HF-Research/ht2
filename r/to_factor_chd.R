##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param dat
##' @param is_totals
##' @param is_sex
##' @param is_age
##' @param plot_var_id
##' @param ui_sex_levels
to_factor_chd <- function(dat = dataObj(), is_totals = isTotals(), is_sex =
                          isSex(), is_age = isAge(), plot_var_id = plotVarId(),
                          ui_sex_levels = ui_sex_levels) {

  x <- copy(dat)
  
  # Turn characters into factors
  if (is_totals) {
    x
  } else if (is_sex) {
    x[, sex := factor(sex,
                      c(levels = "f", "m"),
                      labels = c(ui_sex_levels[1], ui_sex_levels[2]))]
    x[, id_var := paste0(get(plot_var_id[1]))]
  } else if (is_age) {
    x[, id_var := paste0(get(plot_var_id[1]))]
  } else {
    x[, sex := factor(sex,
                      c(levels = "f", "m"),
                      labels = c(ui_sex_levels[1], ui_sex_levels[2]))]
    
  }

}
