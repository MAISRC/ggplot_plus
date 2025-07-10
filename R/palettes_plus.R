#' Makes the Viridis Family of Color Palettes the Default For ggplots
#'
#' This function coerces the default color palettes for the fill and color aesthetics of ggplots to be the `viridis` family of color palettes for the rest of the session by overriding defaults set inside of options(). This ensures the color palettes used with be accessible for those with diverse technological and ocular needs.
#'
#' @param palette_discrete The viridis-family color palette to use for discrete variables. Defaults to "D", which corresponds to the viridis color palette. Must be a length-1 character string from "A" to "H" corresponding to the same codes used by `scale_color_viridis_d()` and similar functions to refer to the eight color families available in the viridis package.
#' @param palette_continuous The viridis-family color palette to use for continuous variables. Defaults to "E", which corresponds to the cividis color palette, which has fewer distinct hues, resulting in less false segmenting of the underlying data driven by hue transitions. Must be a length-1 character string from "A" to "H" corresponding to the same codes used by `scale_color_viridis_d()` and similar functions to refer to the eight color families available in the viridis package.
#' @param begin_discrete The color value of the "low" end of the color palette for a discrete scale. Defaults to 0.28, which is a medium-dark blue in the viridis palette. Must be a single numeric value between 0 and 1.
#' @param end_discrete The color value of the "high" end of the color palette for a discrete scale. Defaults to 0.72, which is a medium-light green in the viridis palette. Must be a single numeric value between 0 and 1.
#' @param begin_continuous The color value of the "low" end of the color palette for a continuous scale. Defaults to 0, which is a dark blue. Must be a single numeric value between 0 and 1.
#' @param end_continuous The color value of the "high" end of the color palette For a continuous scale. Defaults to 1, which is a light yellow. Must be a single numeric value between 0 and 1.
#' @return Does not return an object.
#' @examples
#' palettes_plus()
#' ggplot2::ggplot(iris, ggplot2::aes(x=Sepal.Length, y=Petal.Length, fill=Species)) +
#' geom_plus(geom = "point")
#' @export
palettes_plus = function(palette_discrete = "D",
                         palette_continuous = "E",
                         begin_discrete = 0.28,
                         end_discrete = 0.72,
                         begin_continuous = 0,
                         end_continuous = 1) {

options(
  ggplot2.discrete.fill = function(...) { ggplot2::scale_fill_viridis_d(begin = begin_discrete, end = end_discrete, option = palette_discrete,  ...) },
  ggplot2.discrete.colour = function(...) { ggplot2::scale_colour_viridis_d(begin = begin_discrete, end = end_discrete, option = palette_discrete, ...) },
  ggplot2.continuous.fill = function(...) { ggplot2::scale_fill_viridis_c(begin = begin_continuous, end = end_continuous, option = palette_continuous, ...) },
  ggplot2.continuous.colour = function(...) { ggplot2::scale_colour_viridis_c(begin = begin_continuous, end = end_continuous, option = palette_continuous, ...) }
)
}
