##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param title_text
##' @param var_descriptions_chd
##' @param var_chd
##' @param replace_outcome_string_chd
##' @param ui_replace_all_chd
variable_desc_chd <- function(title_text = prettyVarChd()[1],
                              var_descriptions_chd = var_descriptions_chd,
                              var_chd = input$var_chd,
                              replace_outcome_string_chd = replaceOutcomeStringChd(),
                              ui_replace_all_chd = ui_replace_all_chd) {
  title_text <- tags$b(title_text)
  
  col_selection <- "desc_general"
  desc_text <-
    var_descriptions_chd[code_name == var_chd, ..col_selection]
  
  # Replace sections of variable desc that are specific for
  # outcome/year/outcome-type
  desc_text <-
    gsub("REPLACE_OUTCOME",
         replace_outcome_string_chd,
         (desc_text$desc_general),
         fixed = TRUE)
  
  # For some reason, some danish characters encoding is messed up after the
  # gsub fn(). This fixes that
  desc_text <- gsub("Ã¥", "å", desc_text, fixed = TRUE)
  desc_text <- gsub("Any CHD", ui_replace_all_chd, desc_text)
  
  tagList(title_text, (desc_text))
  
}
