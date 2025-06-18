#' @importFrom ggplot2 ggplot_add
NULL
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
  x_var = if (!is.null(mapping$x)) rlang::as_name(mapping$x) else NULL #RETURN NULL IN CASE THIS IS A GRAPH WITH ONLY ONE MAPPED AESTHETIC, LIKE A HISTOGRAM
  y_var = if (!is.null(mapping$y)) rlang::as_name(mapping$y) else NULL

  #STEP 3: DETERMINE IF ANY VARIABLE IS MAPPED, IF THAT VARIABLE IS IN THE DATA, AND IF IT'S NUMERIC.
  x_is_cont = !is.null(x_var) && x_var %in% names(plot$data) && is.numeric(plot$data[[x_var]])
  y_is_cont = !is.null(y_var) && y_var %in% names(plot$data) && is.numeric(plot$data[[y_var]])

  #STEP 4: IN SOME GRAPH TYPES, A DERIVED NUMERIC AXIS MAY GET BUILT, SUCH AS A HISTOGRAM OR DENSITY PLOT. LET'S CHECK FOR THOSE.
  if (!x_is_cont || !y_is_cont) {

    built = suppressMessages(ggplot2::ggplot_build(plot)) #FAKE-BUILD THE PLOT.

    #LOOK AT THE CLASSES OF ITS SCALES
    x_scales = built$layout$panel_scales_x
    y_scales = built$layout$panel_scales_y

    #CHECK IF EITHER SCALE IS DERIVED BUT STILL CONTINUOUS. WE DO SO BY CHECKING FOR A ScaleContinuous CLASS DESIGNATION ON THE PANEL_SCALES.
    x_is_cont = x_is_cont || any(vapply(x_scales, function(s) inherits(s, "ScaleContinuous"), logical(1)))
    y_is_cont = y_is_cont || any(vapply(y_scales, function(s) inherits(s, "ScaleContinuous"), logical(1)))
  }

  #IF COORD_FLIP IS USED, WE NEED TO FLOP THE LOGIC OF THIS FUNCTION BY SWAPPING THE VALUES ABOVE. I CAN'T JUST ! THEM BOTH BECAUSE THEY COULD BOTH BE TRUE OR BOTH FALSE AND THAT WOULD RESULT IN IMPROPER OUTCOMES.
  if(inherits(plot$coordinates, "CoordFlip")) {
    swap = x_is_cont
    x_is_cont = y_is_cont
    y_is_cont = swap
  }

  #BASELINE NEW THEME CALL.
  grid_theme = ggplot2::theme()

  #STEP 5: IF X IS CONTINUOUS, ADD JUST MAJOR X AXIS GRIDLINES IN THIS DIRECTION.
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
