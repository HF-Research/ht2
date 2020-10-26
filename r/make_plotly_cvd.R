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
  function(x, num_digits, pretty_variable = prettyVariable(),
           sex = ui_sex,
           sex_levels,
           count_rate) {
    
    x[, (sex) := factor(get(sex),
                        levels = c("male", "female"),
                        labels = sex_levels)]
    linesize = 3
    pointsize = 8
    tooltip <-
      paste0("%{text}: <br><b>%{y:,.", num_digits, "f}<extra></extra>")
    
    plot <- plot_ly() %>%
      add_trace(
        data = x,
        x = ~ get(ui_year),
        y = ~ get(pretty_variable),
        color = ~ get(ui_sex),
        colors = (graph_colors),
        text = ~ get(ui_sex),
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = linesize),
        marker = list(size = pointsize),
        hovertemplate = tooltip
      )
    
    if(count_rate == 2){
      return(plot)
    }
    tots <- x[, sum(get(pretty_variable)), by = .(get(ui_year))] %>% 
      setnames( new = c('year', 'v1'))
    plot %>% 
      add_trace(
        data = tots,
        x = ~ year,
        y = ~ v1,
        color = I(single_val_col),
        text = "Total",
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = linesize),
        marker = list(size = pointsize),
        hovertemplate = tooltip,
        name = "Total",
        visible = "legendonly"
        )
  }
