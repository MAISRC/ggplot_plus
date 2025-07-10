#' Generates Subtle and Choice Gridlines on a ggplot
#'
#' This function adds, by default, subtle, light gray major gridlines to a ggplot graph only in directions mapped to continuous (and not discrete) variables.
#'
#' @param color The color used for the gridlines. Defaults to "gray90". Must be a single character vector of length 1 corresponding to the name of a color.
#' @param linewidth Line width for the gridlines. Defaults to 1.2. Must be a single numeric value.
#' @param linetype Line type for the gridlines. Defaults to "solid." Must be a single character string value corresponding to an accepted linetype, such as "dotted" or "dashed".
#' @return List with the class "gridlines_plus", which will trigger the gridlines_plus method in ggplot::ggplot_add.
#' @examples
#' ggplot2::ggplot(iris, ggplot2::aes(x=Sepal.Length, y=Petal.Length)) +
#' geom_plus(geom = "point") +
#' gridlines_plus()
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
#' @param object_name Internal name used by ggplot2 when adding the layer.
#'
#' @return A ggplot object with the new gridlines added.
#' @export
ggplot_add.gridlines_plus = function(object, plot, object_name) {

  #ASSUME BOTH X AND Y ARE NOT CONTINUOUS TO START.
  x_is_cont = FALSE
  y_is_cont = FALSE

  #BECAUSE USERS COULD SPECIFY X AND Y AESTHETICS LOCALLY AND EVEN PROVIDE DATA LOCALLY, WE NEED TO CYCLE THRU THE LAYERS TO FIGURE OUT IF ANY X/Y AESTHETIC IS MAPPED TO A CONTINUOUS VARIABLE AT ANY POINT.
  for(layer in plot$layers) {
    #IF THIS LAYER IS NOT INHERITING DATA FROM GLOBAL, STICK W/ THE LOCAL DATA. OTHERWISE, GO TO GLOBAL.
    if (!inherits(layer$data, "waiver")) {
      layer_data = layer$data
    } else {
      layer_data = plot$data
    }

    #CUT OUT THE SPECIFIC MAPPINGS FOR X AND Y
    x_map = layer$mapping[names(layer$mapping) == "x"]
    y_map = layer$mapping[names(layer$mapping) == "y"]

    #TRY TO GRAB THE VARIABLE LINKED TO EACH MAPPING, ASSUMING THERE IS ONE (THERE MAY NOT BE)
    if (!is.null(x_map)) {
      x_var = rlang::as_label(x_map$x)
    } else {
      x_var = NULL
    }
    if (!is.null(y_map)) {
      y_var = rlang::as_label(y_map$y)
    } else {
      y_var = NULL
    }

    #NOW, CHECK--WAS THERE A VARIABLE? IS THAT VARIABLE IN THIS LAYER? IS IT NUMERIC? WAS A PREVIOUS X/Y VARIABLE IN A PREVIOUS LAYER NUMERIC? UPDATE X/Y_IS_CONT ACCORDINGLY.
    if (!is.null(x_var) && x_var %in% names(layer_data)) {
      x_is_cont = x_is_cont || is.numeric(layer_data[[x_var]])
    }

    if (!is.null(y_var) && y_var %in% names(layer_data)) {
      y_is_cont = y_is_cont || is.numeric(layer_data[[y_var]])
    }
  }

  #STEP 4: IN SOME GRAPH TYPES, A DERIVED NUMERIC AXIS MAY GET BUILT, SUCH AS A HISTOGRAM OR DENSITY PLOT. LET'S CHECK FOR THOSE.
  if (!x_is_cont || !y_is_cont) {

    built = suppressMessages(ggbuild_ggplot(plot)) #FAKE-BUILD THE PLOT.

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
