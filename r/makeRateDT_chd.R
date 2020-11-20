##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param dat
##' @param group_var
##' @param dt_title
##' @param messageBottom
##' @param n_col
##' @param digits
makeRateDT_chd <- function(dat, group_var, dt_title, messageTop, messageBottom, n_col,
                           thousands_sep = thousands_sep,
                           dec_mark = dec_mark,
                           digits) {

  num_format_cols <- (1:n_col)[-1]
  DT::datatable(
    data = dat,
    extensions = 'Buttons',
    rownames = FALSE,
    class = 'hover row-border',
    options = list(
      language = list(url = "Danish.json"),
      ordering = FALSE,
      dom = "tB",
      buttons = list(
        list(extend = "pdf",
             title = dt_title,
             messageTop = messageTop
             ),
        list(extend = "copy",
             title = dt_title,
             messageTop = messageTop
             )
      ),
      initComplete = header_JS
    )
  )  %>%
    formatCurrency(
      columns = num_format_cols,
      currency = "",
      interval = 3,
      mark = thousands_sep,
      dec.mark = dec_mark,
      digits = digits
    ) %>%
    formatStyle(1:n_col, borderColor = "white") %>% 
    formatStyle(group_var,  backgroundColor = DT_background_color, color = "white")
    
}
