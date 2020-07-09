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
makeRateDT_chd <- function(dat, group_var, dt_title, messageBottom, n_col,
                           digits) {

  num_format_cols <- (1:n_col - 1)[-1]
  DT::datatable(
    data = dat,
    extensions = 'Buttons',
    rownames = FALSE,
    class = 'hover row-border',
    options = list(
      language = list(url = "Danish.json"),
      ordering = FALSE,
      dom = "tB",
      columnDefs = list(list(render = formatNAValues, targets = num_format_cols)),
      buttons = list(
        list(extend = "pdf",
             messageTop = dt_title,
             messageBottom = messageBottom),
        list(extend = "excel",
             messageTop = dt_title,
             messageBottom = messageBottom)
      ),
      initComplete = header_JS
    )
  )  %>%
    formatStyle(1:n_col, borderColor = "white") %>% 
    formatStyle(group_var,  backgroundColor = DT_background_color, color = "white")
    
}
