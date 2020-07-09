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
  tick_font_size <- 14
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
    # tmp1 <- rep(ui_sex_levels[1], 2)
    # tmp2 <- rep(ui_sex_levels[2], 2)
    # legend.labs <- paste0(c(tmp1, tmp2), c(" <15", " 15+"))
    # legend.cols <- rep(graph_colors, each = 2)
    #
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
  
  out %>% layout(
    margin = list(t = 60),
    title = list(
      text = plot_title,
      x = 0,
      yanchor = "bottom",
      
      font = list(family = c("Roboto"),
                  size = 25)
    ),
    xaxis = list(title = list(
      text = ui_year,
      font = list(size = axis_font_size)
    ),
    tickfont = list(size = tick_font_size)),
    yaxis = list(
      title = list(text = pretty_var_chd_units,
                   font = list(size = axis_font_size)),
      tickfont = list(size = tick_font_size),
      rangemode = "tozero"),
    hoverlabel = list(font = list(size = 18)),
    hovermode = "x unified",
    separators = paste0(dec_mark, thousands_sep),
    legend = list(
      font = list(size = legend_font_size))
    
  ) %>%
    config(
      locale = "da",
      modeBarButtonsToRemove = c(
        "zoomIn2d",
        "zoomOut2d",
        "zoom2d",
        "pan2d",
        "select2d",
        "lasso2d",
        "autoScale2d",
        "resetScale2d"
      ),
      toImageButtonOptions = list(
        filename = paste0("HjerteTal- ", replace_outcome_string_chd),
        width = 1200,
        height = 600
      )
    )

}
