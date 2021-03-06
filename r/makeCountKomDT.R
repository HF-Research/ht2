##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param dat
##' @param group_var
##' @param thousands_sep
##' @param dt_title
##' @param messageBottom
##' @param n_col
makeCountKomDT <- function(dat,
                           group_var,
                           thousands_sep,
                           dt_title,
                           messageTop,
                           messageBottom,
                           n_col,
                           dec_mark = dec_mark,
                           digits) {
  num_format_cols <- (1:n_col)[-1]
  
  DT::datatable(
    data = dat,
    extensions = 'Buttons',
    rownames = FALSE,
    class = ' hover row-border',
    options = list(
      language = list(url = "Danish.json"),
      ordering = TRUE,
      lengthMenu = list(c(15, 50, -1), c('15', '50', 'Alle')),
      pageLength = 15,
      dom = "lftBsp",
      buttons = list(
        list(
          extend = "pdf",
          title = dt_title,
          messageTop = messageTop,
          messageBottom = messageBottom
        ),
        list(
          extend = "copy",
          title = dt_title,
          messageTop = messageTop,
          messageBottom = messageBottom
        )
      ),
      initComplete = header_JS(DT_background_color, text_color = DT_text_color)
    )
  ) %>%
    formatCurrency(
      columns = num_format_cols,
      currency = "",
      interval = 3,
      mark = thousands_sep,
      dec.mark = dec_mark,
      digits = digits
    ) %>%
    formatStyle(1:n_col, borderColor = "white") %>%
    formatStyle('Total',  fontWeight = 'bold') %>%
    formatStyle(group_var,  fontWeight = 'bold') %>%
    formatStyle(
      # Bolds the "Totals" row which has character == "Total" in column 1
      1,
      target = "row",
      fontWeight = styleEqual(
        levels = c("Total"),
        values =  c("bold"),
        default = "normal"
      )
    )
  
  
}
