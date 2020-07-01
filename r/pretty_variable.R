pretty_variable <- function(lang, data_var_name, selected_rate_type, var_ui){
  var_lang <- paste0("var_", lang)
  grep_selection <-
    paste0("var_rate_", selected_rate_type, "_", lang)
  col_names <- colnames(var_ui)
  col_selection <- grep(grep_selection, col_names, value = TRUE)
  c(var_ui[code_name == data_var_name, get(var_lang)],
    paste0(var_ui[code_name == data_var_name, ..col_selection]))
  
  
}
