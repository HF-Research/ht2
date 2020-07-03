##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param dat
##' @param col_names
##' @param order
##' @param paging
##' @param dom
##' @param search
makeAboutTables <- function(dat, col_names, order = FALSE, paging = FALSE, dom
                            = "Bt", search = FALSE) {

  colnames(dat) <- col_names
  DT::datatable(
    data = dat,
    extensions = 'Buttons',
    rownames = FALSE,
    class = ' hover row-border',
    selection = "multiple",
    options = list(
      ordering = order,
      paging = paging,
      searching = search,
      pageLength = 20,
      dom = dom,
      buttons = list('pdf'),
      initComplete = header_JS,
      autoWidth = TRUE
      # columnDefs = list(list(targets = c(0), visible = TRUE, width= '35%'))
    )
  )

}
