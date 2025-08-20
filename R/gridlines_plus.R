#' Subtle, Minimal Gridlines For When and Where They Help
#'
#' Adds light, major gridlines along only axes that are mapped to
#' continuous variables (never on discrete axes). Minor gridlines are
#' blanked. This enables the benefits of gridlines but minimizes clutter.
#' Works with local/global mappings, derived continuous axes
#' (e.g., histograms), and respects `coord_flip()`.
#'
#' @param color Gridline color. Single character string. Default: `"gray90"`.
#' @param linewidth Gridline width (theme line units). Single numeric. Default: `1.2`.
#' @param linetype Gridline type. Single string (e.g., `"solid"`, `"dashed"`).
#'
#' @return An object of class `"gridlines_plus"` that you add to a plot with `+`.
#'
#' @details
#' Under the hood, `gridlines_plus()` checks layer and/or global mappings to
#' see if `x` and/or `y` are continuous. If needed (e.g., for derived axes like
#' histograms), it briefly builds the plot to inspect the trained panel scales.
#' It then turns on **major** gridlines for continuous directions and explicitly
#' blanks gridlines for the others (including **all minor** gridlines).
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Petal.Length)) +
#'   ggplot2::geom_point() +
#'   gridlines_plus()
#'
#' # Only y is continuous here (x is discrete) → y-only major gridlines
#' ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) +
#'   ggplot2::geom_boxplot() +
#'   gridlines_plus(color = "grey85", linewidth = 1, linetype = "dashed")
#'
#' # Works with derived continuous axes (histogram)
#' ggplot2::ggplot(mtcars, aes(mpg)) +
#'   ggplot2::geom_histogram() +
#'   gridlines_plus()
#'
#' @export
gridlines_plus = function(color = "gray90",
                   linewidth = 1.2,
                   linetype = "solid") {
  structure(
    list(color = color, linewidth = linewidth, linetype = linetype),
    class = "gridlines_plus"
  )
}

#' @title Add Gridlines_plus Theme Adjustments To A Ggplot
#' @description S3 method for adding a `gridlines_plus` object via `+`.
#'   Detects continuous axes and applies major gridlines there; blanks all
#'   other gridlines. Internal wiring; users should call
#'   [gridlines_plus()] rather than this method.
#'
#' @param object A `gridlines_plus` object created by [gridlines_plus()].
#' @param plot A ggplot object.
#' @param object_name Internal name used by ggplot2.
#'
#' @return A ggplot object with theme tweaks applied.
#'
#' @keywords internal
#' @export
#' @method ggplot_add gridlines_plus
ggplot_add.gridlines_plus = function(object, plot, object_name) {

    plot = .ensure_ggplot_plus(plot) #RUN THIS TO MANAGE STORAGE AND CLASSES.
    plot$ggplot_plus$grid = list( #STASH IN SUB-OBJECT INTENTS.
      color = object$color,
      linewidth = object$linewidth,
      linetype = object$linetype
    )
    plot #RETURN

}
