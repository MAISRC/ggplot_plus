palettes_plus = function(palette = "D", begin = 0.28, end = 0.72) {

options(
  ggplot2.discrete.fill = function(...) { scale_fill_viridis_d(begin = begin, end = end, option = palette,  ...) },
  ggplot2.discrete.colour = function(...) { scale_colour_viridis_d(begin = begin, end = end, option = palette, ...) },
  ggplot2.continuous.fill = function(...) { scale_fill_viridis_c(begin = begin, end = end, option = palette, ...) },
  ggplot2.continuous.colour = function(...) { scale_colour_viridis_c(begin = begin, end = end, option = palette, ...) }
)
}
