##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param pretty_var_singular
##' @param ui_sex
##' @param pretty_aggr_level
##' @param count_rate
##' @param subset_year
##' @param
data_d3_bar <-
  function(pretty_var_singular = prettyVariableSingular(),
           ui_sex = ui_sex,
           pretty_aggr_level = prettyAggr_level(),
           count_rate = count_rate,
           subset_year = subsetYear()
  ) {
    count_rate <- pretty_var_singular
    keep_cols <- c(ui_sex, pretty_aggr_level, count_rate)
    dat <- subset_year[, ..keep_cols]
    dat <- dat[, (count_rate) := lapply(.SD, function(i) {
      # Any NA values need to be converted to 0s to be sent to d3
      i[is.na(i)] <- 0
      i
    }),
    .SDcols = count_rate]
    
    # For variables that present PERCENTAGE results - divide by 1000
    
    # Order so that males come first - makes sure the coloring matches
    setorderv(dat, ui_sex, order = -1L)
    dat[]
    
  }
