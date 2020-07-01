##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param aggr_selected
##' @param outcome_code
##' @param variables_not_used
##' @param lang
##' @param selected_var
validate_selected_vars <- function(aggr_selected = input$aggr_level,
                                   outcome_code = outcomeCode(),
                                   variables_not_used = variables_not_used,
                                   lang = lang,
                                   selected_var = selected_var) {
  
  outcome_subset <- shiny_dat[[outcome_code]][[aggr_selected]]
  var_names <- valid_output_combos[outcome == outcome_code &
                                     aggr_level == aggr_selected, unique(shiny_code)]
  
  # Remove columns with data that should not be shown to user
  var_names <- var_names[!var_names %in% variables_not_used]
  
  # Select the plain language terms matching the variables in data
  var_lang <- paste0("var_", lang)
  keep_vars <- c("shiny_code", var_lang)
  variable_choices <-
    variable_ui[shiny_code %in% var_names, ..keep_vars]
  var_names <- variable_choices$shiny_code
  names(var_names) <- variable_choices[[var_lang]]
  
  # On start, var_selected is NULL, so set default value of validate_selection
  # to TRUE, so tables are shown
  validate_selection <- TRUE
  
  logic <- !(selected_var %in% var_names)
  if (length(logic) > 0 && logic == TRUE) {
    validate_selection <- FALSE
  }
  
  list(
    selected_var = selected_var,
    var_names = var_names,
    valid_selection = validate_selection
  )
  
}
