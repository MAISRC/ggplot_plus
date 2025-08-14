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

  #ASSUME BOTH X AND Y ARE NOT CONTINUOUS TO START.
  x_is_cont = FALSE
  y_is_cont = FALSE

  gmap = plot$mapping #THIS WOULD BE THE GLOBALLY MAPPING--FALL BACK TO THIS IF LAYERS DON'T SPECIFY THEM.

  #BECAUSE USERS COULD SPECIFY X AND Y AESTHETICS LOCALLY AND EVEN PROVIDE DATA LOCALLY, WE NEED TO CYCLE THRU THE LAYERS TO FIGURE OUT IF *ANY* X/Y AESTHETIC IS MAPPED TO A CONTINUOUS VARIABLE IN *ANY* LAYER.
  for(layer in plot$layers) {
    #IF THIS LAYER IS NOT INHERITING DATA FROM GLOBAL, STICK W/ THE LOCAL DATA. OTHERWISE, FALL BACK TO GLOBAL.
    if(!inherits(layer$data, "waiver")) {
      layer_data = layer$data
    } else {
      layer_data = plot$data
    }

    #RESOLVE WHETHER WE'RE DEALING WITH LOCAL OR GLOBAL MAPPINGS FOR X AND Y
    lmap = layer$mapping #LOCAL MAPPINGS
    x_quo = if(!is.null(lmap$x)) { lmap$x } else gmap$x
    y_quo = if(!is.null(lmap$y)) { lmap$y } else gmap$y

    #GET NAMES OF VARS MAPPED TO X AND Y AESTHETICS. IF THEY ARE EXPRESSIONS, WE CATCH THAT VIA BUILD LATER
    x_var = if(!is.null(x_quo)) { rlang::as_label(x_quo) } else { NULL }
    y_var = if(!is.null(y_quo)) { rlang::as_label(y_quo) } else { NULL }

    #NOW, CHECK--WAS THERE A VARIABLE? IS THAT VARIABLE IN THIS LAYER? IS IT NUMERIC? WAS A PREVIOUS X/Y VARIABLE IN A PREVIOUS LAYER NUMERIC? UPDATE X/Y_IS_CONT ACCORDINGLY.
    if(!is.null(x_var) && x_var %in% names(layer_data)) {
      x_is_cont = x_is_cont || is.numeric(layer_data[[x_var]])
    }

    if(!is.null(y_var) && y_var %in% names(layer_data)) {
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

  #STEP 5: IF AN AXIS IS CONTINUOUS, ADD MAJOR AXIS GRIDLINES IN THIS DIRECTION.
  grid_theme = ggplot2::theme(
    panel.grid.major.x = if(x_is_cont) { ggplot2::element_line(color = object$color,
                                                              linewidth = object$linewidth,
                                                              linetype = object$linetype)
    } else { ggplot2::element_blank() },
    panel.grid.major.y = if(y_is_cont) { ggplot2::element_line(color = object$color,
                                                              linewidth = object$linewidth,
                                                              linetype = object$linetype)
    } else { ggplot2::element_blank() }
  )


  #DRAW THE PLOT WITH THE NEW GRIDLINES.
  plot + grid_theme + ggplot2::theme(panel.grid.minor = element_blank())
}
