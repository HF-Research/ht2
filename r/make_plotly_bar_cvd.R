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
make_plotly_bar_cvd <-
  function(x,
           num_digits = numDigitsCVD(),
           pretty_variable = prettyVariableSingular(),
           pretty_aggr_level = prettyAggr_level(),
           sex = ui_sex,
           sex_levels) {
    tooltip <-
      paste0("%{text}: <br><b>%{y:,.", num_digits, "f}<extra></extra>")
    
    x[, (sex) := factor(get(sex),
                        levels = c("male", "female"),
                        labels = sex_levels)]
    x[, (pretty_aggr_level) := html_wrap_v(get(pretty_aggr_level), 20)]
    x %>%
      plot_ly() %>%
      add_trace(
        x = ~ get(pretty_aggr_level),
        y = ~ get(pretty_variable),
        text = ~ get(sex),
        color = ~ get(sex),
        colors = (graph_colors),
        type = "bar",
        hovertemplate = tooltip
      )
    
    
    
  }
