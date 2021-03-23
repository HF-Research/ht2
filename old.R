# output$varChoices <- renderUI({
#   req(input$agCVD)
#   
#   # Gives a dynamic button UI.
#   #
#   # This next code allows the variable chosen by the user to remain, when
#   # switching to a new outcome, while on a aggr_level not supported with the new
#   # outcome. F.x. Switch from all-CVD, 30-day mortality, kommune-level, to
#   # hjerteklapoperation. Hjerteklaoperation only supports 30-day mort at
#   # national level, so the variable is switched to incidence.
#   #
#   # If the previous selected var is not available, test to see if it is
#   # available in the previously selected aggr_level. If not to both, set
#   # selected_var to be the first variable.
#   aggr_selected_next <-
#     isolate(aggrButtonChoices()$selected_aggr)
#   var_choice_out <-
#     make_var_choices(
#       selected_var = (validateSelectedVars()$selected_var),
#       var_names = (validateSelectedVars()$var_names),
#       valid_selection = (validateSelectedVars()$valid_selection),
#       aggr_selected_next = aggr_selected_next,
#       outcome_code = input$oCVD,
#       valid_output_combos = valid_output_combos
#     )
#   
#   selectInput(
#     inputId = "varCVD",
#     label = choose_var,
#     choices = var_choice_out$var_names,
#     selectize = TRUE,
#     selected = var_choice_out$selected_var
#   )
# })



# 
# aggrButtonChoices <- reactive({
#   # Dynamically chanages which aggre_level options are available depending on
#   # which outcome and which variable is selected
#   input_aggr_level <- isolate(input$agCVD)
#   
#   ag_choice_out <- make_agg_choices(
#     var_selected = input$varCVD,
#     outcome_code = input$oCVD,
#     valid_output_combos = valid_output_combos,
#     aggr_choices = aggr_choices,
#     input_aggr_level = input_aggr_level
#   )
#   
#   if (is.null(ag_choice_out))
#     return(NULL)
#   
#   html_output <- radioGroupButtons(
#     inputId = "agCVD",
#     label = choose_aggr_lv,
#     choices = ag_choice_out$button_vals,
#     justified = TRUE,
#     direction = "vertical",
#     selected = ag_choice_out$selected_aggr
#   )
#   
#   return(
#     list(
#       button_vals = ag_choice_out$button_vals,
#       selected_aggr = ag_choice_out$selected_aggr,
#       html_output = html_output
#     )
#   )
# })


# output$aggrButtonChoices <- renderUI({
#   aggrButtonChoices()$html_output
# })
