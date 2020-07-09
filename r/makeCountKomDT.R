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
makeCountKomDT <- function(dat, group_var, thousands_sep, dt_title,
                           messageBottom, n_col) {

  num_format_cols <- (1:n_col - 1)[-1]
  
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
      columnDefs = list(list(render = formatSuppressedValues, targets = num_format_cols)),
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
  ) %>%
    formatStyle(1:n_col, borderColor = "white") %>% 
    formatStyle('Total',  fontWeight = 'bold') %>%
    formatStyle(group_var,  backgroundColor = DT_background_color, color = "white") %>%
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
