##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param selected_var
##' @param var_names
##' @param valid_selection
##' @param aggr_selected_next
##' @param outcome_code
##' @param valid_output_combos
make_var_choices <- function(selected_var =
                               validateSelectedVars()$selected_var,
                             var_names =
                               validateSelectedVars()$var_names,
                             valid_selection
                             = validateSelectedVars()$valid_selection,
                             aggr_selected_next = aggr_selected_next,
                             outcome_code = outcomeCode(),
                             valid_output_combos
                             = valid_output_combos) {
  
  if (is.null(selected_var)) {
    # If no previous selection:
    selected_var <- var_names[1]
  } else if (!valid_selection) {
    # If selected var not in current selection AND...
   
    if (is.null(aggr_selected_next)) {
      aggr_selected_next <- "national"
    }
    # Variable available for aggr_selected_next and outcome
    var_names_2 <-
      valid_output_combos[outcome == outcome_code &
                            aggr_level == aggr_selected_next,
                          unique(shiny_code)]
    if (!selected_var %in% var_names_2) {
      # ...selected var also not in set of vars attached to previously
      # selected aggr_level: Set var to incidence
      selected_var <- var_names[1]
    } else {
      var_names <- var_names_2
    }
  }
  return(list(var_names = var_names,
              selected_var = selected_var))
}
