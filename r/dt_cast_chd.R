##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param x
##' @param pretty_var_chd_units
##' @param is_totals
##' @param is_sex
##' @param is_age
##' @param ui_year
##' @param ui_sex_levels
dt_cast_chd <- function(x = dataObj(), pretty_var_chd_units =
                        prettyVarChdUnits(), is_totals = isTotals(), is_sex =
                        isSex(), is_age = isAge(), ui_year = ui_year,
                        ui_sex_levels = ui_sex_levels) {

  dat <- copy(x)
  if (is_totals) {
    setnames(dat, c(ui_year, "Total"))
    dat
  } else if (is_sex) {
    value_var <- pretty_var_chd_units
    subset_cols = c("year", value_var)
    out = cbind(dat[sex == "f", ..subset_cols], dat[sex == "m", ..value_var])
    setnames(out, c(ui_year, ui_sex_levels[1], ui_sex_levels[2]))
  } else if (is_age) {
    value_var <- pretty_var_chd_units
    subset_cols = c("year", value_var)
    out = cbind(dat[age_adult == "<18", ..subset_cols], dat[age_adult == "18+", ..value_var])
    setnames(out, c(ui_year, "<18", "18+"))
  } else {
    
    # Id_var created, and key set, in data pre-processing step 
    value_var <- pretty_var_chd_units
    subset_cols = c("year", value_var)
    out <-
      cbind(dat["f<18", ..subset_cols],
            dat["f18+", ..value_var],
            dat["m<18", ..value_var],
            dat["m18+", ..value_var])
    
    tmp1 <- rep(ui_sex_levels[1], 2)
    tmp2 <- rep(ui_sex_levels[2], 2)
    var_names <- paste0(c(tmp1, tmp2), c(" <18", " 18+"))
    setnames(out, c(ui_year, var_names))
    out
  }

}
