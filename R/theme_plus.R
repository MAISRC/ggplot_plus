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
theme_plus = function(..., legend_pos = c("top", "right")) {

  user_args = list(...)
  structure(list(user_args = user_args,
            legend_pos = match.arg(legend_pos)),
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

  theme2add = determine_legend_theme(legend_pos) #GET LEGEND_SPECIFIC THEME ASPECTS.

  #ADD THE THEMES TO THE PLOT, WITH THE USER'S PROVIDED SPECIFICATIONS LAST SO THOSE WOULD OVERRIDE ANY UPSTREAM FALLBACKS.
  plot + default_theme + theme2add + do.call(ggplot2::theme, user_args)
}


#' A Convenience Function to Access `ggplot.plus`'s Opinionated Default Theme
#'
#' This function provides a convenient way to extract the package’s opinionated
#' default theme, including legend-position-specific settings.
#' Use the returned theme like you would any other `ggplot2` theme (see examples).

#' @param legend_pos Character string (`"top"` or `"right"`).
#'   Controls where the legend is placed and which theme settings are applied.
#'   Defaults to `"top"`. Any other value will be interpretted as `"right"`.
#'
#' @return A `ggplot2::theme` object.
#'
#' @seealso [theme_plus()] for the full theme-building workflow.
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = factor(cyl))) +
#'   geom_plus(geom = "point") +
#'   get_theme_plus()                  # legend on top (default)
#'
#' ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = factor(cyl))) +
#'   geom_plus(geom = "point") +
#'   get_theme_plus("right")           # legend on the right
#'
#' ggplot2::theme_set(get_theme_plus()) # set theme to session default
#' @export
get_theme_plus = function(legend_pos = "top") {
  default_theme + determine_legend_theme(legend_pos)
}
