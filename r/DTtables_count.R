##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param dat
##' @param ag_lv
##' @param pretty_ag_lv
##' @param pretty_vars
##' @param sex_levels
##' @param ui_count_rate
##' @param thousands_sep
##' @param dec_mark
##' @param plot_title
##' @param is_kom
DTtables_count <- function(dat = dtCast(), ag_lv = input$aggr_level,
                           pretty_ag_lv = prettyAggr_level(), pretty_vars =
                           prettyVariable(), sex_levels = ui_sex_levels,
                           ui_count_rate = ui_count_rate, thousands_sep =
                           thousands_sep, dec_mark = dec_mark, plot_title =
                           plotTitle(), is_kom = isKom()) {

  vars <-
    c("group_var",
      grep(pretty_vars[1], colnames(dat), value = TRUE))
  
  dat <- dat[, ..vars]
  colnames(dat) <- c("group_var", "male", "female")
  # Calculate margins
  dat[, Total := rowSums(dat[, .(female, male)], na.rm = TRUE)]
  if (ag_lv != "national") {
    # Only calculate bottom margins for "age" - other aggr levels don't include
    # full data
    totals <-
      dat[, colSums(dat[, .(male, female, Total)], na.rm = TRUE)]
    
    # Convert entire table to character to we can rbind() totals
    dat <- dat[, lapply(.SD, as.character)]
    # Rbind totals
    dat <- rbindlist(list(dat, as.list(c("Total", totals))), use.names = FALSE)
    
    # Convert back to numeric
    col_convert <- c("male", "female", "Total")
    dat[, (col_convert) := lapply(.SD, as.numeric), .SDcols = col_convert]
    
  }
  
  setnames(
    dat,
    old = c("group_var", "male", "female"),
    new = c(pretty_ag_lv, rev(sex_levels))
  )
  
  col_convert <- c(sex_levels, "Total")
  n_col <- NCOL(dat)
  if (is_kom) {
    makeCountKomDT(
      dat,
      group_var = pretty_ag_lv,
      thousands_sep = thousands_sep,
      dt_title = plot_title,
      messageBottom = paste0(ui_count_rate[1], " ", tolower(pretty_vars[1])),
      n_col = n_col
    )
  } else {
    
    makeCountDT(
      dat,
      group_var = pretty_ag_lv,
      thousands_sep = thousands_sep,
      dt_title = plot_title,
      messageBottom = paste0(ui_count_rate[1], " ", tolower(pretty_vars[1])),
      n_col = n_col
    ) %>% 
      formatCurrency(columns = col_convert, currency = "", interval = 3, mark = thousands_sep, dec.mark = dec_mark, digits = 0)
  }
  

}
