##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param plot_title
##' @param axis_font_size
##' @param tick_font_size
##' @param legend_font_size
##' @param axis_title_x
##' @param axis_title_y
##' @param dec_mark
##' @param thousands_sep
##' @param ...
##' @return
##' @author Matthew Phelps
##' @export
plotly_config_line <- function(x,plot_title = plot_title, axis_font_size,
                               tick_font_size, legend_font_size =
                               legend_font_size, axis_title_x, axis_title_y,
                               dec_mark = dec_mark, thousands_sep =
                               thousands_sep, file_suffix) {

  x %>% layout(
    margin = list(t = 60),
    title = list(
      text = plot_title,
      x = 0,
      yanchor = "bottom",
      
      font = list(family = c("Roboto"),
                  size = 25)
    ),
    xaxis = list(title = list(
      text = axis_title_x,
      font = list(size = axis_font_size)
    ),
    tickfont = list(size = tick_font_size)),
    yaxis = list(
      title = list(text = axis_title_y,
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
        filename = paste0("HjerteTal- ", file_suffix),
        width = 1000,
        height = 500, scale = "2"
      )
    )

}
