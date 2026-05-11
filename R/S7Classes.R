#' Internal S7 class for gridline intent
#'
#' Stores user-specified gridline styling from `gridlines_plus()` so it can be
#' applied after ggplot2 has trained the plot scales.
#'
#' @keywords internal
#' @noRd
GridlinesPlus = S7::new_class(
  "GridlinesPlus",
  properties = list(
    color = S7::class_character,
    linewidth = S7::class_numeric,
    linetype = S7::class_character,
    notx = S7::class_logical,
    noty = S7::class_logical
  )
)

#' Internal S7 class for theme_plus() additions
#'
#' Stores the completed ggplot2 theme object produced by `theme_plus()` and
#' whether geom-specific default adjustments should be applied during plot
#' building.
#'
#' @keywords internal
#' @noRd
ThemePlus = S7::new_class(
  "ThemePlus",
  properties = list(
    applyGeomDefaults = S7::class_logical,
    theme2add = S7::class_any
  )
)

#' Internal S7 class for y-axis title intent
#'
#' Stores user intent from `yaxis_title_plus()` so the y-axis title can be moved
#' during gtable construction, after ggplot2 has assembled the plot layout.
#'
#' @keywords internal
#' @noRd
YAxisTitlePlus = S7::new_class(
  "YAxisTitlePlus",
  properties = list(
    location = S7::class_character,
    nudgeTopLegendDown = S7::class_logical,
    nudgeHowMuch = S7::class_numeric)
)

#' Internal S7 state container for ggplotplus plots
#'
#' Stores deferred ggplotplus intents attached to a plot. This object is carried
#' on `GGPlotPlusPlot` objects and read during build and gtable stages.
#'
#' @keywords internal
#' @noRd
GGPlotPlusState = S7::new_class(
  "GGPlotPlusState",
  properties = list(
    grid = S7::new_property(class = S7::class_any, default = NULL),
    y_axis_title = S7::new_property(class = S7::class_any, default = NULL),
    theme = S7::new_property(class = S7::class_any, default = NULL),
    guides = S7::new_property(class = S7::class_any, default = NULL),
    warnings = S7::new_property(class = S7::class_any, default = NULL)
  )
)

#' Internal S7 ggplot subclass for ggplotplus dispatch
#'
#' Subclasses ggplot2 plot objects so ggplotplus can dispatch custom build-stage
#' behavior without changing ggplot2 behavior globally.
#'
#' @keywords internal
#' @noRd
GGPlotPlusPlot = S7::new_class(
  "GGPlotPlusPlot", #IT'S SIGNATURE, OR NAME, IS GGPlotPlusPlot
  parent = ggplot2::class_ggplot, #THIS IS A SUBCLASS OF CLASS "ggplot"
  properties = list(
    ggplotplus = S7::class_any
  )
)

#' Internal S7 built-plot subclass for ggplotplus gtable dispatch
#'
#' Subclasses ggplot2 built plot objects so ggplotplus can pass deferred
#' gtable-stage intents, such as `yaxis_title_plus()`, from plot building into
#' gtable construction.
#'
#' @keywords internal
#' @noRd
GGPlotPlusBuilt = S7::new_class(
  "GGPlotPlusBuilt",
  parent = ggplot2::class_ggplot_built,
  properties = list(
    y_axis_title = S7::class_any
  )
)
