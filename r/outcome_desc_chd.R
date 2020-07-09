##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param pretty_ouctome_chd
##' @param outcome_description_chd
##' @param oCHD
##' @param ui_read_more
outcome_desc_chd <- function(pretty_ouctome_chd = prettyOutcomeChd(),
                             outcome_description_chd = outcome_description_chd,
                             oCHD = input$oCHD, ui_read_more = ui_read_more) {

  out_title <- tags$b(pretty_ouctome_chd)
  keep_vars <- c("desc", "link")
  out <-
    outcome_descriptions_chd[ht.code == oCHD, ..keep_vars]
  # Add link for further reading - if link exists, otherwise just desc
  url <- a(ui_read_more,
           href = (out$link),
           target = "_blank")
  
  if (!is.na(out$link)) {
    tagList(out_title, out$desc, url)
  }
  else {
    tagList(out_title, out$desc)
  }

}
