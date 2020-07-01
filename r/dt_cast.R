##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param dat
##' @param group_var
##' @param value_var
##' @param is_national
##' @param is_5Year_Mortality
dt_cast <-
  function(dat = outputCasesData(),
           group_var = prettyAggr_level(),
           value_var = prettyVariable(),
           is_national = isNational(),
           is_5year_mortality = is5YearMortality()) {
    subset_cols = c(group_var, value_var)
    out = cbind(dat["male", ..subset_cols], dat["female", ..value_var])
    setnames(out, c("group_var", paste0(value_var, rep(
      c("_male", "_female"), c(2, 2)
    ))))
    if (is_national && is_5year_mortality) {
      return(out[group_var <= year_max - 4, ])
      
    } else {
      return(out)
    }
    
    
  }
