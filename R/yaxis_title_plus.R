#' Relocate a Y Axis Title to Above the Y Axis on a ggplot and Turn it Horizontal.
#'
#' This function relocates the y axis title of a ggplot graph to the top of the plot, above the y axis line and left-justified to the left edge of the y axis labels, sort of like a plot subtitle. It also orients the text horizontally for space-efficiency and easy reading. This is otherwise difficult to do using `ggplot2`'s default styling tools.
#'
#' @param location A length-1 character string matching either "top" or "bottom" for the placement of the new y axis title. Defaults to `"top"`. `"bottom"` should generally only be used when the x axis labels (which would occupy the same row as the new y axis title) have been moved to the top of the graph.
#' @param move_top_legend_down A length-1 logical indicating whether a top legend (if any) should be moved down to be in the same row as the relocated y axis title (where they could clip into each other). Defaults to FALSE.
#'
#' @details
#' This function should be called after calling your respective geom in your call.
#'
#' @return Returns a list of class "axis_switcher", which will trigger the ggplot_add method by the same name.
#' @examples
#' ggplot2::ggplot(iris, ggplot2::aes(x=Sepal.Length, y=Petal.Length)) +
#' geom_plus(geom = "point") +
#' yaxis_title_plus()
#' @export
yaxis_title_plus = function(location = c("top", "bottom"),
                            move_top_legend_down = FALSE) {
  structure(
    list(location = match.arg(location, c("top", "bottom")),
         move_top_legend_down = isTRUE(move_top_legend_down)),
    class = "yaxis_title_plus"
  )
}

#' Initiate the Process of Moving the Y Axis Title to the Top of a ggplot Graph
#'
#' This method defines how objects of class `axis_switcher`, created by the `y_axis_title_plus()` function, are added to a ggplot2 plot using the `+` operator.
#' The method begins the process of rebuilding the ggplot with the y axis title moved to its new location within the gtable.
#'
#' @param object An object of class `axis_switcher`, created by `y_axis_title_plus()`, containing user-provided arguments (if any) or else pre-defined default values that determine where to move the y axis title to.
#' @param plot A ggplot object for which the y axis title should be moved.
#' @param object_name Internal name used by ggplot2 when adding the layer. Defaults to "switcher" so that this class is added to the resulting object.
#'
#' @return A ggplot with the class of "switcher" to trigger the ggplot_build method of the same name and also with the `y_axis_switch_location` attribute set by the call to `y_axis_title_plus()`.
#' @export
ggplot_add.yaxis_title_plus = function(object, plot, object_name) {

  plot = .ensure_ggplot_plus(plot) #MANAGE CLASSES AND STORAGE SPACE.
  plot$ggplot_plus$yaxis = list( #RECORD INTENTS
    location = object$location,
    move_top_legend_down = object$move_top_legend_down
  )
  plot

}
