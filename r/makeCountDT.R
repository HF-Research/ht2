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
makeCountDT <-
  function(dat,
           group_var,
           dt_title,
           messageTop,
           messageBottom,
           n_col,
           thousands_sep,
           dec_mark = dec_mark) {
    num_format_cols <- (1:n_col)[-1]
    
    DT::datatable(
      data = dat,
      extensions = 'Buttons',
      rownames = FALSE,
      class = ' hover row-border',
      options = list(
        language = list(url = "Danish.json"),
        ordering = FALSE,
        dom = "tB",
        
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
        initComplete = header_JS
      )
    ) %>%
      formatCurrency(
        columns = num_format_cols,
        currency = "",
        interval = 3,
        mark = thousands_sep,
        dec.mark = dec_mark,
        digits = 0
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
