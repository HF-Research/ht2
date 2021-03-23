##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param dat
##' @param pretty_vars
##' @param percent_rate
##' @param pretty_ag_lv
##' @param ui_sex_levels
##' @param plot_title
##' @param pretty_var_singular
##' @param is_kom
##' @param thousands_sep
##' @param dec_mark
DTtables_rate <-
  function(dat = dtCast(),
           pretty_vars = prettyVariable(),
           percent_rate = selectPercentOrRate(),
           pretty_ag_lv = prettyAggr_level(),
           ui_sex_levels = ui_sex_levels,
           plot_title = plotTitle(),
           pretty_var_singular = prettyVariableSingular(),
           is_kom = isKom(),
           thousands_sep = thousands_sep,
           dec_mark = dec_mark) {
    # Subset to either counts or rates
    vars <-
      c("group_var",
        grep(
          pretty_vars[2],
          colnames(dat),
          fixed = TRUE,
          value = TRUE
        ))
    dat <- dat[, ..vars]
    colnames(dat) <- c("group_var", "female", "male")
    
    # If results are percentages - round with 1 sig fig. If results are rates -> 0
    # sig figs.
    if (!percent_rate) {
      digits = 0
    } else {
      digits = 1
    }
    
    col_convert <- c(ui_sex_levels)
    n_col <- NCOL(dat)
    setnames(
      dat,
      old = c("group_var", "male", "female"),
      new = c(pretty_ag_lv, ui_sex_levels)
    )
    
    # dat[, (col_convert) := lapply(.SD, function(i){
    #   i[i==0] <- NA
    #   i
    # }), .SDcols = col_convert]
    
    if (is_kom) {
      makeRateKomDT(
        dat = dat,
        group_var = pretty_ag_lv,
        dt_title = plot_title,
        messageTop= "HjerteTal",
        messageBottom = pretty_var_singular,
        n_col = n_col,
        thousands_sep = thousands_sep,
        dec_mark = dec_mark,
        digits = digits
      ) 
        
      
    } else {
      makeRateDT(
        dat = dat,
        group_var = pretty_ag_lv,
        dt_title = plot_title,
        messageTop= "HjerteTal", 
        messageBottom = pretty_var_singular,
        n_col = n_col,
        thousands_sep = thousands_sep,
        dec_mark = dec_mark,
        digits = digits
      )
    }
    
    
  }
