##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param title_text
##' @param lang
##' @param selected_rate_type
##' @param selected_data_vars
##' @param replace_outcome_string
##' @param replace_agg_level_string
##' @param replace_type_string
##' @param replace_allCVD_string
##' @param input_year
##' @param is_national
text_rate_desc <-
  function(title_text = prettyVariable()[2],
           lang = lang,
           selected_rate_type = selectedRateType(),
           selected_data_vars = selectedDataVars(),
           replace_outcome_string = replaceOutcomeString(),
           replace_agg_level_string = replaceAggrLevelString(),
           replace_type_string = replaceTypeString(),
           input_year = input$year,
           is_national = isNational()) {
    
    title_text <- tags$b(title_text)
    col_selection <-
      paste0("desc_", selected_rate_type, "_", lang)
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
    
    
    # Fix Danish letters
    desc_text <- gsub("Ã¥", "å", desc_text, fixed = TRUE)
    
    desc_text <-
      gsub("REPLACE_TYPE", replace_type_string, desc_text, useBytes = TRUE)
    desc_text <-
      gsub("REPLACE_YEAR", tolower(input_year), desc_text, useBytes = TRUE)
    
    # Fix danish only issues
    if (is_national && lang == "dk") {
      desc_text <- gsub("for hver", "for hvert", desc_text, fixed = TRUE)
      desc_text <-
        gsub("i en given", "i et givet", desc_text, fixed = TRUE)
      desc_text <- gsub("rr", "r", desc_text, fixed = TRUE)
    }
    
    tagList(title_text, desc_text)
    
  }
