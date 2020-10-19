##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param x
##' @param plot_title
##' @param num_digits
##' @param thousands_sep
##' @param dec_mark
##' @param is_totals
##' @param is_sex
##' @param is_age
##' @param pretty_var_chd_units
##' @param replace_outcome_string_chd
make_plotly_chd <- function(x = toFactor(), plot_title = dataTitle(),
                            num_digits = numDigits(), thousands_sep =
                            thousands_sep, dec_mark = dec_mark, is_totals =
                            isTotals(), is_sex = isSex(), is_age = isAge(),
                            pretty_var_chd_units = prettyVarChdUnits(),
                            replace_outcome_string_chd =
                            replaceOutcomeStringChd()) {

  axis_font_size <- 20
  legend_font_size <- 17
  tick_font_size <- 15
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
    
  } else if (is_sex) {
    out <- plot_ly(data = x, x = ~ year) %>%
      add_trace(
        y = ~ get(pretty_var_chd_units),
        color = ~ id_var,
        colors = rev(graph_colors),
        
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = linesize),
        marker = list(size = pointsize)
      )
  } else if (is_age) {
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
  } else {
    
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
    
  }
  
  plotly_config_line(x = out, plot_title = plot_title,
             axis_font_size = axis_font_size,
             tick_font_size = tick_font_size,
             legend_font_size = legend_font_size,
             axis_title_x = ui_year,
             axis_title_y = pretty_var_chd_units,
             dec_mark = dec_mark,
             thousands_sep = thousands_sep,
             file_suffix = replace_outcome_string_chd
             )
  
  

}
