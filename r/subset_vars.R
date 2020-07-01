subset_vars <- function(dat, data_vars, ag_lv, select_raw_mean, select_percent_rate, pretty_variable,pretty_ag_lv){
  # Subsets the original dataset so that only the needed columns and rows are showing
  
  # Switch between RAW and MOVNIG AVG data
  # raw == TRUE, 3-yr moving avg == FALSE
  if (select_raw_mean) {
    data_vars <- data_vars[!grepl("mean", data_vars)]
  } else {
    data_vars <- data_vars[grepl("mean", data_vars)]
  }
  
  col_vars <- c("year", "sex", "grouping", data_vars)
  dat <- dat[, ..col_vars]
  
  # Select based on aggre_level
  if (ag_lv != "national") {
    setnames(dat,
             c(ui_year, ui_sex, pretty_ag_lv, pretty_variable))
  } else {
    setnames(dat, c(ui_year, ui_sex, "age", pretty_variable))
  }
  
  if (select_percent_rate) {
    var_to_modify <- grep(ui_percent, names(dat), value = TRUE)
    dat[, (var_to_modify) := round(get(var_to_modify) / 1000, digits = 1)]
  }
  dat[]
}