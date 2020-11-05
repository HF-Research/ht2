pretty_variable <- function(lang, var_shiny_code, selected_rate_type, var_ui){
  var_lang <- paste0("var_", lang)
  grep_selection <-
    paste0("var_rate_", selected_rate_type, "_", lang)
  col_names <- colnames(var_ui)
  col_selection <- grep(grep_selection, col_names, value = TRUE)
  c(var_ui[shiny_code == var_shiny_code, get(var_lang)],
    paste0(var_ui[shiny_code == var_shiny_code, ..col_selection]))
  
  
}
