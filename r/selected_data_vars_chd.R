##' .. content for \description{Returns the column names to be used to subset the data - taking into account raw or mean data} () ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param var_chd
##' @param subset_outcome_chd
##' @param rate_count
selected_data_vars_chd <-
  function(var_chd = input$var_chd,
           subset_outcome_chd = subsetOutcomeChd(),
           rate_count = input$rate_count_chd) {
  
    var_stripped <- gsub("count_n_|rate_", "", var_chd)
    var_stripped <- paste0(var_stripped, "$") # make sure no matching with multiple input vars
    tmp <-
      grep(var_stripped, colnames(subset_outcome_chd), value = TRUE)
    tmp[as.integer(rate_count)]
    
  }
