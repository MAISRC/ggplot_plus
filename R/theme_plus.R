#' Add a New Base Theme to ggplots With Elevated Defaults
#'
#' Wrapper function for ggplot2's `theme()` function that still allows users to specify custom values for theme attributes but has default values for many attributes that are more likely to result in a graph that meets best practices for design aesthetics, usability, and accessibility.
#'
#' @param ... Other arguments to be passed along to the `theme()` (optional).
#' @return List with the class "theme_plus", which will trigger the theme_plus method in ggplot_add.
#' @examples
#' ggplot(iris, aes(x=Sepal.Length, y=Petal.Length)) + geom_plus(geom = "point") + theme_plus()
#' @export
theme_plus = function(...) {

  user_args = list(...)
  structure(list(user_args = user_args),
            class = "theme_plus")
}

#' Add A theme_plus-generated theme to a ggplot
#'
#' This method defines how objects of class `theme_plus()`, added by the theme_plus function, are added to a ggplot2 plot using the `+` operator.
#' It applies user-specified overrides to sensible default values and ensures compatibility with ggplot2 layering.
#'
#' @param object An object of class `theme_plus`, created by `theme_plus()`, containing user-provided arguments (if any) and otherwise default values for many theme attributes.
#' @param plot A ggplot object to which the new thene will be applied
#' @param name Internal name used by ggplot2 when adding the theme.
#'
#' @return A ggplot object with the new theme applied.
#' @export
ggplot_add.theme_plus = function(object, plot, name) {

  user_args = object$user_args #EXTRACT THE USER'S ARGUMENTS

  #DEFINE THE DEFAULT THEME HERE.
  default_theme = ggplot2::theme(
  # aspect.ratio = 1, #I WANT TO DO MORE RESEARCH ON WHAT THIS VALUE SHOULD BE OR HOW TO CALCULATE IT.
  axis.line = ggplot2::element_line(color = "black", linewidth = 1.2), #ADD THICK BLACK X AND Y AXIS LINES
  axis.title.x = ggplot2::element_text(color = "black", size = 18, margin = ggplot2::margin(t = 10)), #ADD TOP MARGIN TO X AXIS TITLE.
  axis.title.y = ggplot2::element_text(color = "black", size = 18, vjust = 0.25, margin = ggplot2::margin(r  = 15)),
  axis.text = ggplot2::element_text(size = 16, color = "black"), #ENSURE AXIS LABELS ARE BLACK AND SIZE 16
  axis.ticks.length = unit(0.3, "cm"), #INCREASE SIZE OF AXIS TICK MARKS TO BE MORE NOTICEABLE.
  legend.position = "top",
  legend.direction = "horizontal",
  legend.title = ggplot2::element_text(color = "black", size = 18, margin = ggplot2::margin(r = 15)),
  legend.box.just = "bottom",
  legend.key.spacing.x = ggplot2::unit(0.5, "cm"),
  legend.text = ggplot2::element_text(size = 16, color = "black"),
  legend.justification = "right",
  legend.key = ggplot2::element_rect(fill = "transparent", color = "white"),
  legend.background = ggplot2::element_rect(color = "white", fill = "white"),
  legend.ticks.length = ggplot2::unit(0.3, "cm"),
  legend.frame = ggplot2::element_rect(color = "black", linewidth = 1.2),
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
  plot + default_theme + do.call(theme, user_args)
}
