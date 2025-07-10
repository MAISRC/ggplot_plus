#' Add a New Base Theme to ggplots With Elevated Defaults
#'
#' Wrapper function for ggplot2's `theme()` function that still allows users to specify custom values for theme attributes but has default values for many attributes that are more likely to result in a graph that meets best practices for design aesthetics, usability, and accessibility.
#'
#' @param ... Other arguments to be passed along to the `theme()` (optional).
#' @param legend_pos Where should the legend(s) be? Defaults to "top", which will make the legend a horizontal stripe at the top of the graph. Any other value will move the legend to a vertical stripe to the right of the plot (its usual position in `ggplot2`).
#' @return List with the class "theme_plus", which will trigger the theme_plus method in ggplot_add.
#' @examples
#' ggplot2::ggplot(iris, ggplot2::aes(x=Sepal.Length, y=Petal.Length)) +
#' geom_plus(geom = "point") + theme_plus()
#' @export
theme_plus = function(..., legend_pos = "top") {

  user_args = list(...)
  structure(list(user_args = user_args,
            legend_pos = legend_pos),
            class = "theme_plus")
}

#' Add A theme_plus-generated theme to a ggplot
#'
#' This method defines how objects of class `theme_plus()`, added by the theme_plus function, are added to a ggplot2 plot using the `+` operator.
#' It applies user-specified overrides to sensible default values and ensures compatibility with ggplot2 layering.
#'
#' @param object An object of class `theme_plus`, created by `theme_plus()`, containing user-provided arguments (if any) and otherwise default values for many theme attributes.
#' @param plot A ggplot object to which the new theme will be applied
#' @param object_name Internal name used by ggplot2 when adding the theme.
#'
#' @return A ggplot object with the new theme applied.
#' @export
ggplot_add.theme_plus = function(object, plot, object_name) {

  user_args = object$user_args #EXTRACT THE USER'S ARGUMENTS
  legend_pos = object$legend_pos #EXTRACT LEGEND POS DESIRES.

  if(!is.null(legend_pos) &&
     legend_pos == "top") {
    theme2add = ggplot2::theme(
      legend.key.width = ggplot2::unit(1.5, "cm"),
      #GREATLY EXPAND THE WIDTH
      legend.key.height = ggplot2::unit(0.75, "cm"),
      #EXPAND THE HEIGHT A BIT ALSO.)
      legend.title = ggplot2::element_text(margin = ggplot2::margin(r = 15), vjust = 0.5),
      legend.box.just = "bottom",
      legend.justification = "right",
      legend.key.spacing.x = ggplot2::unit(0.5, "cm"),
      legend.position = "top",
      legend.direction = "horizontal"
    )
  } else {
    theme2add = ggplot2::theme(legend.key.height = ggplot2::unit(1.5, "cm"),
                      legend.key.width = ggplot2::unit(0.75, "cm"),
                      legend.title = ggplot2::element_text(margin = ggplot2::margin(b = 15), hjust = 0.5),
                      legend.box.just = "right",
                      legend.justification = "right",
                      legend.key.spacing.y = ggplot2::unit(0.5, "cm"),
                      legend.position = "right",
                      legend.direction = "vertical")
  }


  #DEFINE THE DEFAULT THEME HERE.
  default_theme = ggplot2::theme(
  # aspect.ratio = 1, #I WANT TO DO MORE RESEARCH ON WHAT THIS VALUE SHOULD BE OR HOW TO CALCULATE IT.
  axis.line = ggplot2::element_line(color = "black", linewidth = 1.2, lineend = "square"), #ADD THICK BLACK X AND Y AXIS LINES WITH SQUARE ENDS TO ENSURE THAT THEY APPEAR TO VISUALLY MEET.
  axis.title.x = ggplot2::element_text(color = "black", size = 18, margin = ggplot2::margin(t = 10)), #ADD TOP MARGIN TO X AXIS TITLE.
  axis.title.y = ggplot2::element_text(color = "black", size = 18, vjust = 0.25, margin = ggplot2::margin(r  = 15)),
  axis.text = ggplot2::element_text(size = 16, color = "black"), #ENSURE AXIS LABELS ARE BLACK AND SIZE 16
  axis.ticks.length = ggplot2::unit(0.3, "cm"), #INCREASE SIZE OF AXIS TICK MARKS TO BE MORE NOTICEABLE.
  legend.title = ggplot2::element_text(color = "black", size = 18),
  legend.text = ggplot2::element_text(size = 16, color = "black"),
  legend.key = ggplot2::element_rect(fill = "transparent", color = "white"),
  legend.background = ggplot2::element_rect(color = "white", fill = "white"),
  legend.ticks.length = ggplot2::unit(0.2, "cm"),
  legend.frame = ggplot2::element_rect(color = "black", linewidth = 1.2), #MAKE SOLID BLACK LINES FOR THE LEGEND BORDER FOR CONTINUOUS SCALES.
  legend.ticks = ggplot2::element_line(color = "white", linewidth = 1.2, linetype = "solid"), #MAKE THE TICKS WHITE
  panel.border = ggplot2::element_blank(),
  panel.grid = ggplot2::element_blank(), #ELIMINATE MAJOR AND MINOR GRIDLINES
  panel.background = ggplot2::element_rect(fill = "white"), #SWITCH FROM GRAY TO WHITE BACKGROUND
  panel.spacing = ggplot2::unit(1, "cm"),
  plot.title = ggplot2::element_blank(),
  plot.subtitle = ggplot2::element_blank(),
  strip.background = ggplot2::element_rect(color = "white", fill = "white"),
  strip.text = ggplot2::element_text(color = "black", size = 16, face = "bold"),
  strip.text.y = ggplot2::element_text(angle = 0)
)

  #ADD THE THEMES TO THE PLOT, WITH THE USER'S PROVIDED SPECIFICATIONS SECOND SO THEY OVERRIDE ANY RELEVANT DEFAULTS.
  plot + default_theme + theme2add + do.call(ggplot2::theme, user_args)
}
