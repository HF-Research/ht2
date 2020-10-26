##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param x
##' @param num_digits
##' @param is_totals
##' @param is_sex
##' @param is_age
##' @param pretty_var_chd_units

make_plotly_chd <- function(x = toFactor(), 
                            num_digits,
                            is_totals = isTotals(),
                            is_sex = isSex(), is_age = isAge(),
                            pretty_var_chd_units = prettyVarChdUnits()
                            ) {

  
  linesize = 3
  pointsize = 8
  tooltip <-
    paste0("<b>%{y:,.", num_digits, "f}")
  
  if (is_totals) {
    tooltip <-
      paste0(pretty_var_chd_units, ": <br><b>%{y:,.", num_digits, "f}</b><extra></extra>")
    out <-  plot_ly(data = x, x = ~ year) %>%
      add_trace(
        y = ~ get(pretty_var_chd_units),
        type = 'scatter',
        mode = 'lines+markers',
        color = I(single_val_col),
        line = list(width = linesize),
        marker = list(size = pointsize),
        hovertemplate = tooltip
      )
    return(out)
    
  }
  if (is_sex) {
    
    out <- plot_ly(data = x, x = ~ year) %>%
      add_trace(
        y = ~ get(pretty_var_chd_units),
        color = ~ id_var,
        colors = rev(graph_colors),
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = linesize),
        marker = list(size = pointsize),
        hovertemplate = tooltip
      )
    return(out)
  }
  if (is_age) {
    out <- plot_ly(data = x, x = ~ year) %>%
      add_trace(
        y = ~ get(pretty_var_chd_units),
        linetype = ~ age_adult,
        color = I(single_val_col),
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = linesize),
        marker = list(size = pointsize),
        hovertemplate = tooltip
      )
    return(out)
  }
    
  # AGE SEX
    setorder(x, year)
    out <- plot_ly(data = x) %>%
      add_trace(
        x = ~ year,
        y = ~ get(pretty_var_chd_units),
        color = ~ sex,
        colors = rev(graph_colors),
        linetype = ~ age_adult,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = linesize),
        marker = list(size = pointsize),
        hovertemplate = tooltip
        
      )
    
    
  return(out)
  
  
  

}
