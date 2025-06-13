#' Generates Subtle and Choice Gridlines on a ggplot
#'
#' This function adds, by default, subtle, light gray major gridlines to a ggplot graph only in directions mapped to continuous (and not discrete) variables.
#'
#' @param color The color used for the gridlines. Defaults to "gray90". Must be a single character vector of length 1 corresponding to the name of a color.
#' @param linewidth Line width for the gridlines. Defaults to 1.2. Must be a single numeric value.
#' @param linetype Line type for the gridlines. Defaults to "solid." Must be a single character string value corresponding to an accepted linetype, such as "dotted" or "dashed".
#' @return List with the class "gridlines_plus", which will trigger the gridlines_plus method in ggplot::ggplot_add.
#' @examples
#' ggplot(iris, aes(x=Sepal.Length, y=Petal.Length)) + geom_plus(geom = "point") + gridlines_plus()
#' @export
gridlines_plus = function(color = "gray90",
                   linewidth = 1.2,
                   linetype = "solid") {
  structure(
    list(color = color, linewidth = linewidth, linetype = linetype),
    class = "gridlines_plus"
  )
}

#' Add A gridlines_plus-generated Geometry to a ggplot
#'
#' This method defines how objects of class `gridlines_plus()`, added by the gridlines_plus function, are added to a ggplot2 plot using the `+` operator.
#' It considers both default values as well as user overrides for important gridlines features and ensures compatibility with ggplot2 layering.
#'
#' @param object An object of class `gridlines_plus`, created by `gridlines_plus()`, containing user-provided arguments (if any) or else pre-defined default values.
#' @param plot A ggplot object to which the new gridlines should be added.
#' @param name Internal name used by ggplot2 when adding the layer.
#'
#' @return A ggplot object with the new gridlines added.
#' @export
ggplot_add.gridlines_plus = function(object, plot, name) {
  #STEP 1: GET THE MAPPING FOR THE PROVIDED PLOT.
  mapping = plot$mapping

  #STEP 2: GET CLASSES OF VARIABLES FOR X AND Y AXES.
  x_var = rlang::as_name(mapping$x)
  y_var = rlang::as_name(mapping$y)

  #STEP 3: DETERMINE IF NUMERIC.
  x_is_cont = is.numeric(plot$data[[x_var]])
  y_is_cont = is.numeric(plot$data[[y_var]])

  #BASELINE NEW THEME CALL.
  grid_theme = ggplot2::theme()

  #IF X IS CONTINUOUS, ADD JUST MAJOR X AXIS GRIDLINES IN THIS DIRECTION.
  if (x_is_cont) {
    grid_theme = grid_theme + ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(
        color = object$color,
        linewidth = object$linewidth,
        linetype = object$linetype
      )
    )
  }
  #SAME FOR Y
  if (y_is_cont) {
    grid_theme = grid_theme + ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(
        color = object$color,
        linewidth = object$linewidth,
        linetype = object$linetype
      )
    )
  }
  #DRAW THE PLOT WITH THE NEW GRIDLINES.
  plot + grid_theme
}
