##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param dat
##' @param map_obj
##' @param mini_map_lines
##' @param pretty_variable
##' @return
##' @author Matthew Phelps
##' @export
make_static_map <- function(dat,
                            map_obj,
                            mini_map_lines = dk_sp$mini_map_lines,
                            pretty_variable = prettyVariable()[2],
                            plot_title = plotTitle(),
                            thousands_sep = thousands_sep,
                            dec_mark = dec_mark,
                            sex) {
  z <- map_obj
  
  mini_map_lines <- as.data.table(mini_map_lines)
  legend_title <- pretty_variable %>%
    stringr::str_wrap(width = 25) %>%
    paste0(., "\n")
  
  fill_var <- sym(pretty_variable)
  dat_range <- range(dat, na.rm = TRUE)
  mid_point <- dat_range[2] - ((dat_range[2] - dat_range[1]) / 2)
  
  ggplot() +
    geom_sf(data = z, aes(fill = !!fill_var), color = "grey35") +
    scale_fill_gradientn(
      # low = "#FFFFCC",
      # mid = "#FD8D3C",
      # midpoint = mid_point,
      # high = "#800026",
      colors = RColorBrewer::brewer.pal(9, name = "YlOrRd"),
      limits = dat_range,
      labels = function(x)
        format(
          x,
          big.mark = thousands_sep,
          decimal.mark = dec_mark,
          scientific = FALSE
        )
    ) +
    geom_segment(
      data = mini_map_lines,
      color = "grey",
      size = 1.2,
      aes(
        x = mini_map_lines[name == "bottom_left"]$X1,
        xend = mini_map_lines[name == "bottom_right"]$X1,
        y = mini_map_lines[name == "bottom_right"]$X2,
        yend = mini_map_lines[name == "bottom_right"]$X2
      )
    ) +
    geom_segment(
      data = mini_map_lines,
      color = "grey",
      size = 1.2,
      aes(
        x = mini_map_lines[name == "bottom_left"]$X1,
        xend = mini_map_lines[name == "bottom_left"]$X1,
        y = mini_map_lines[name == "bottom_left"]$X2,
        yend = mini_map_lines[name == "top_left"]$X2
      )
    ) +
    annotate(
      "text",
      x = mini_map_lines$X1[3] + 0.5,
      y = mini_map_lines$X2[3] + 0.934,
      label = "Credit: HjerteTal.dk",
      size = 3.5,
      fontface = "italic",
      color = "grey30"
    ) +
    theme_void() +
    labs(
      fill = legend_title,
      title = stringr::str_wrap(plot_title, 55),
      subtitle = sex
    ) +
    
    theme(
      legend.position = c(0.82, 0.84),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 15),
      plot.title = element_text(size = 19)
    ) +  guides(fill = guide_colourbar(barwidth = 1,
                                       barheight = 10))
  
}
