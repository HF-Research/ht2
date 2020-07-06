##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param title_text
##' @param lang
##' @param variable_ui
##' @param selected_data_vars
##' @param replace_outcome_string
##' @param replace_type_string
text_var_desc <- function(title_text = prettyVariable()[1], lang = lang, variable_ui = variable_ui,
                          selected_data_vars = selectedDataVars()[1],
                          replace_outcome_string = replaceOutcomeString(),
                          replace_type_string = replaceTypeString(),
                          replace_allCVD_string = replace_allCVD_string) {

  title_text <- tags$b(title_text)
  
  col_selection <- paste0("desc_general_", lang)
  desc_text <-
    variable_ui[code_name == selected_data_vars[1], ..col_selection]
  
  # Replace sections of variable desc that are specific for
  # outcome/year/outcome-type
  
  desc_text[1] <-
    gsub(
      "REPLACE_OUTCOME",
      replace_outcome_string,
      (desc_text[1]),
      fixed = TRUE
    )
  # For some reason, some danish characters encoding is messed up after the
  # gsub fn(). This fixes that
  
  desc_text <- gsub("Ã¥", "å", desc_text, fixed = TRUE)
  desc_text <- gsub("alle hjerte-kar-sygdomme", replace_allCVD_string, desc_text)
  
  
  
  desc_text <-
    gsub("REPLACE_TYPE", replace_type_string, desc_text, useBytes = TRUE)
  
  
  tagList(title_text, (desc_text))

}
