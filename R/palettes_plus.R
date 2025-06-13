#' Makes the Viridis Family of Color Palettes the Default For ggplots
#'
#' This function coerces the default color palettes for the fill and color aesthetics of ggplots to be the `viridis` family of color palettes for the rest of the session by overriding defaults set inside of options(). This ensures the color palettes used with be accessible for those with diverse technological and occular needs. 
#'
#' @param palette The viridis-family color palette to use. Defaults to "D", which corresponds to the viridis color palette. Must be a length-1 character string from "A" to "H" corresponding to the same codes used by `scale_color_viridis_d()` and similar functions to refer to the eight color families available in the viridis package. 
#' @param begin The color value of the "low" end of the color palette. Defaults to 0.28, which is a medium-dark blue in the viridis palette. Must be a single numeric value between 0 and 1.
#' @param end The color value of the "high" end of the color palette. Defaults to 0.72, which is a medium-light green in the viridis palette. Must be a single numeric value between 0 and 1.
#' @return Does not return an object. 
#' @examples
#' palettes_plus(); ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, fill=Species)) + geom_plus(geom = "point")
#' @export
palettes_plus = function(palette = "D", begin = 0.28, end = 0.72) {

options(
  ggplot2.discrete.fill = function(...) { scale_fill_viridis_d(begin = begin, end = end, option = palette,  ...) },
  ggplot2.discrete.colour = function(...) { scale_colour_viridis_d(begin = begin, end = end, option = palette, ...) },
  ggplot2.continuous.fill = function(...) { scale_fill_viridis_c(begin = begin, end = end, option = palette, ...) },
  ggplot2.continuous.colour = function(...) { scale_colour_viridis_c(begin = begin, end = end, option = palette, ...) }
)
}
