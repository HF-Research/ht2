##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param var_selected
##' @param outcome_code
##' @param valid_output_combos
##' @param aggr_choices
##' @param input_aggr_level
make_agg_choices <-
  function(var_selected = input$varCVD,
           outcome_code = outcomeCode(),
           valid_output_combos = valid_output_combos,
           aggr_choices = aggr_choices,
           input_aggr_level = input_aggr_level) {
    valid_output_combos <-
      valid_output_combos[outcome == outcome_code]
    
    # When app first starts, input$varCVD will be null, but need to get range of
    if (is.null(var_selected))
      var_selected <- valid_output_combos[1, shiny_code]
    
    
    aggr_level_choices <-
      valid_output_combos[outcome == outcome_code &
                            shiny_code == var_selected, unique(aggr_level)]
    
    # When switching between d, b, and m outcomes, this will return NULL at first calling
    if (length(aggr_level_choices) == 0)
      return(NULL)
    
    aggr_choices <- aggr_choices[name_ht %in% aggr_level_choices]
    row.names(aggr_choices) <- aggr_choices$label
    button_vals <-
      setNames(split(aggr_choices$name_ht, seq(nrow(aggr_choices))),
               row.names(aggr_choices))
    
    
    # If the previous selected aggr_level is available in the new outcome vars, make that
    # the default, else the first variable
    selected_aggr <- input_aggr_level
    if (is.null(selected_aggr) ||
        !selected_aggr %in% aggr_choices$name_ht) {
      selected_aggr <- aggr_choices$name_ht[1]
    }
    
    return(list(button_vals = button_vals,
                selected_aggr = selected_aggr))
    
  }
