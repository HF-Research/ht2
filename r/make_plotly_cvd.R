##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param x
##' @param num_digits
##' @param pretty_variable
##' @return
##' @author Matthew Phelps
##' @export
make_plotly_cvd <-
  function(x, num_digits, pretty_variable = prettyVariable()) {
    linesize = 3
    pointsize = 8
    tooltip <-
      paste0("%{text}: <br><b>%{y:,.", num_digits, "f}<extra></extra>")
    plot_ly(data = x, x = ~ get(ui_year)) %>%
      add_trace(
        y = ~ get(pretty_variable),
        color = ~ get(ui_sex),
        colors = rev(graph_colors),
        text = ~ get(ui_sex),
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = linesize),
        marker = list(size = pointsize),
        hovertemplate = tooltip
      )
    
  }
