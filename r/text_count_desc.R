##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param pretty_var
##' @param ui_count_rate
##' @param lang
##' @param selected_data_vars
##' @param replace_outcome_string
##' @param replace_agg_level_string
##' @param replace_type_string
##' @param input_year
text_count_desc <-
  function(pretty_var = prettyVariable(),
           ui_count_rate =
             ui_count_rate,
           lang = lang,
           selected_data_vars =
             selectedDataVars(),
           replace_outcome_string =
             replaceOutcomeString(),
           replace_agg_level_string =
             replaceAggrLevelString(),
           replace_type_string =
             replaceTypeString(),
           input_year = input$year) {
    title_text <-
      tags$b(paste0(ui_count_rate[1], " ", tolower(pretty_var[1])))
    
    col_selection <- paste0("desc_", "count", "_", lang)
    desc_text <-
      variable_ui[code_name == selected_data_vars[1], ..col_selection]
    desc_text <-
      gsub("REPLACE_OUTCOME",
           replace_outcome_string,
           desc_text,
           fixed = TRUE)
    
    desc_text <-
      gsub("REPLACE_AGGR",
           replace_agg_level_string,
           desc_text,
           fixed = TRUE)
    desc_text <- gsub("Ã¥", "å", desc_text, fixed = TRUE)
    
    desc_text <-
      gsub("REPLACE_TYPE", replace_type_string, desc_text, useBytes = TRUE)
    desc_text <-
      gsub("REPLACE_YEAR", tolower(input_year), desc_text, useBytes = TRUE)
    
    
    
    tagList(title_text, desc_text)
    
  }
