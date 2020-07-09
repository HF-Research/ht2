##' .. content for \description{} (Returns the column names to be used to subset the data - taking into account
# raw or mean data) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param varCVD
##' @param variable_ui
##' @param subset_outcome
selected_data_vars <- function(varCVD = input$varCVD, variable_ui =
                               variable_ui, subset_outcome = subsetOutcome()) {

  
  var_code_cvd <- variable_ui[shiny_code == varCVD, code_name]
  var_stripped <- gsub("count_|rate_", "", var_code_cvd)
  grep_str <- paste0(var_stripped, "$")
  grep(grep_str, colnames(subset_outcome), value = TRUE)  

}
