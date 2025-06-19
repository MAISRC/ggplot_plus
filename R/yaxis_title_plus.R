#' @importFrom ggplot2 ggplot_add ggplot_build ggplot_gtable
NULL
#' Relocate a Y Axis Title to Above the Y Axis on a ggplot and Turn it Horizontal.
#'
#' This function relocates the y axis title of a ggplot graph to the top of the plot, above the y axis line and left-justified to the left edge of the y axis labels, sort of like a plot subtitle. It also orients the text horizontally for space-efficiency and easy reading. This is otherwise difficult to do using `ggplot2`'s default styling tools.
#'
#' @param location A length-1 character string matching either "top" or "bottom" for the placement of the new y axis title. Defaults to `"top"`. `"bottom"` should generally only be used when the x axis labels (which would occupy the same row as the new y axis title) have been moved to the top of the graph.
#' @return Returns a list of class "axis_switcher", which will trigger the ggplot_add method by the same name.
#' @examples
#' ggplot(iris, aes(x=Sepal.Length, y=Petal.Length)) + geom_plus(geom = "point") + y_axis_title_plus()
#' @export
yaxis_title_plus = function(location = "top") {
  structure(
    list(location = match.arg(location, c("top", "bottom"))),
    class = "axis_switcher"
  )
}

#' Initiate the Process of Moving the Y Axis Title to the Top of a ggplot Graph
#'
#' This method defines how objects of class `axis_switcher`, created by the `y_axis_title_plus()` function, are added to a ggplot2 plot using the `+` operator.
#' The method begins the process of rebuilding the ggplot with the y axis title moved to its new location within the gtable.
#'
#' @param object An object of class `axis_switcher`, created by `y_axis_title_plus()`, containing user-provided arguments (if any) or else pre-defined default values that determine where to move the y axis title to.
#' @param plot A ggplot object for which the y axis title should be moved.
#' @param name Internal name used by ggplot2 when adding the layer. Defaults to "switcher" so that this class is added to the resulting object.
#'
#' @return A ggplot with the class of "switcher" to trigger the ggplot_build method of the same name and also with the `y_axis_switch_location` attribute set by the call to `y_axis_title_plus()`.
#' @export
ggplot_add.axis_switcher = function(object, plot, name = "switcher") {
  plot$y_axis_switch_location = object$location
  class(plot) = c("switcher", class(plot))
  plot
}

#' Build a ggplot With the Class "switcher".
#'
#' This method defines how objects of class `switcher`, created by the `ggplot_add.axis_switcher()` function, are built into a ggplot2 plot.
#' The method continues the process of rebuilding the ggplot with the y axis title moved to its new location within the gtable.
#'
#' @param plot A ggplot object for which the y axis title should be moved.
#'
#' @return A ggplot with the class of "switched" to trigger the ggplot_gtable method of the same name and also with the `y_axis_switch_location` attribute set by the call to `y_axis_title_plus()`.
#' @export
ggplot_build.switcher = function(plot) {
  class(plot) = setdiff(class(plot), "switcher")
  output = suppressMessages(ggplot2::ggplot_build(plot))
  class(output) = c("switched", class(output))
  output
}

#' Finish a ggplot With the Class "switched".
#'
#' This method defines how objects of class `switched`, created by the `ggplot_build.switcher()` function, are finalized into a ggplot2 plot.
#' The method finishes the process of rebuilding the ggplot with the y axis title moved to its new location within the gtable.
#'
#' @param plot A ggplot object with the class of "switched" for which the y axis title should be moved.
#'
#' @return A ggplot object compatible with `ggplot2`'s + command structure.
#' @export
ggplot_gtable.switched = function(plot) {
  loc = ifelse(!is.null(plot$plot$y_axis_switch_location),
                plot$plot$y_axis_switch_location,
                "top") #IF USER DIDN'T SPECIFY DIFFERENT, MOVE THE Y AXIS TITLE TO THE TOP.
  plot = switch_axis_label(plot$plot, location = loc)
  plot + ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 0))) #ADD IN A THEME TO SWITCH HOW THE MARGIN IS ADJUSTED.
}

#****
translate_element = function(el) {
  el_list = as.list(el)
  #TRANSLATE BETWEEN SOME DIFFERENT AESTHETICS DEPENDING ON HOW EXACTLY THEY ARE CODED.
  if (!is.null(el_list$colour)){ el_list$col = el_list$colour }
  if (!is.null(el_list$face)){ el_list$fontface = el_list$face }
  if (!is.null(el_list$size)){ el_list$fontsize = el_list$size }

  return(el_list)
}

#****
element_to_gpar = function(el) {
  el_list = translate_element(el)
  #ATTRIBUTES THAT GPARS NEED TO HAVE--LINE THESE UP WITH THE ATTRIBUTES OF THE ELEMENT BEING PORTED IN.
  gpar_args = c("col", "fill", "alpha", "lty", "lwd", "lex", "lineend",
                "linejoin", "linemitre", "fontsize", "cex", "fontfamily",
                "fontface", "font", "lineheight")
  do.call(grid::gpar, el_list[intersect(names(el_list), gpar_args)])
}

#' Place a Y Axis Title on a ggplot in a Safe Place Above the Y Axis Line.
#'
#' This function relocates the y axis title of a ggplot graph to the top of the plot, above the y axis line and left-justified to the left edge of the y axis labels, sort of like a plot subtitle. It also orients the text horizontally for space-efficiency and easy reading. This is otherwise difficult to do using `ggplot2`'s default styling tools. This is the main function used by `y_axis_title_plus()` to ultimately accomplish its purpose.
#' This function is used internally by the `ggplot_gtable.switched()` method and is not intended for separate use.
#'
#'#' @param p A ggplot object whose y axis title will be moved.
#' @param location A length-1 character string matching either "top" or "bottom" for the placement of the new y axis title. Defaults to `"top"`. Potentially overridden by whatever is specified to `y_axis_title_plus()`'s parameter of the same name when it's called.
#' @return A ggplot object compatible with `ggplot2`'s + command structure.
#' @export
switch_axis_label = function(p, location = "top") {

  #IF A USER USES coord_flip(), I.E., THE WORST GGPLOT2 FUNCTION EVER LOL, THEN WE *REALLY* NEED TO GRAB THE X AXIS TITLE INSTEAD.
  #THE p$coordinates OBJECT WILL HAVE CLASS "CoordFlip" IN THAT INSTANCE.

  if(inherits(p$coordinates, "CoordFlip")) {
    real_scale = "x"
    title_element = "axis.title.x"
  } else {
    real_scale = "y"
    title_element = "axis.title.y"
  }

  y_scale = p$scales$get_scales(real_scale)

  #DEFAULT TO THE SCALE LABEL, IF ANY, AS THIS WILL BE WHAT THE USER HAS HOPEFULLY SET.
  if (!is.null(y_scale)) {
    name = y_scale$name
    if (!is.null(name) && !inherits(name, "waiver") && nzchar(name)) {
      lab = name
    }
  }

  if(!exists("lab")) { #WE FAILED ABOVE, SO WE ENTER HERE IF SO.

  #OTHERWISE, FALL BACK TO THE PLOT LABEL IF ANY, AS THIS WILL GENERALLY BE WHATEVER THE DEFAULT VALUE SET BY GGPLOT WAS.
  label = p$labels[[real_scale]]
  if (!is.null(label) && !inherits(label, "waiver") && nzchar(label)) {
    lab = label
  } else {
    #OTHERWISE, FALL BACK TO SOMETHING GENERIC THAT THE USER WILL KNOW THEY NEED TO REPLACE.
    lab = "Replace with a scale() function!"
   }
  }

  # #ALL THEME-RELATED ADJUSTMENTS MUST BE PORTED OVER MANUALLY. HERE, WE PORT OVER SIZE, TAKING EITHER A CUSTOM SIZE FROM THE PROVIDED THEME, IF ANY, OR ELSE THE SIZE FROM THE DEFAULT THEME. A SIMILAR MODEL COULD BE USED FOR CARRYING OVER THINGS LIKE FONT COLOR AND STYLE.

  #IF YOU'VE SPECIFIED A NEW Y SCALE TITLE VALUE VIA A SCALE_Y_ FUNCTION, THIS NUKES IT.
  if(!is.null(p$scales$get_scales(real_scale))) {
    y = which(unlist(lapply(p$scales$scales, function(x) { real_scale %in% x$aesthetics } )))
    p$scales$scales[[y]]$name = NULL
  }

  #THIS ALSO NUKES THE DEFAULT Y AXIS TITLE STRING.
  if(real_scale == "y") {
  p = p + ggplot2::labs(y = NULL)
  } else {
    p = p + ggplot2::labs(x = NULL)
  }

  #IF A USER IS PLOTTING ON THE BOTTOM INSTEAD OF THE TOP, LET'S AUTO-ADJUST THE VERTICAL ALIGNMENT:
  if(location == "bottom") {
    p = p + theme(axis.title.y = element_text(vjust = 0.75)) #THE DEFAULT OF VJUST = 0.25 WORKS GREAT FOR THE TOP POSITION ALREADY.
  } else {
    p = p + theme(axis.title.y = element_text(vjust = 0.25))
  }

  #NOW, WE CONVERT THE GGPLOT WE ALREADY HAVE INTO A GTABLE.
  gt = ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))

    #HERE, WE ATTEMPT TO PORT OVER ANY THEME-RELATED ADJUSTMENTS TO THE APPEARANCE OF THE Y AXIS TITLE.
  element = ggplot2::calc_element(title_element, p$theme) #GRAB THE ELEMENT'S CURRENT THEME CHARACTERISTICS

  #USUALLY, WE TARGET ROW [8,6] BUT CAN INSTEAD TARGET [11,6] IF THE USER WANTS AND WE LACK A BOTTOM X AXIS LABEL ROW.
  target_row = ifelse(location == "bottom", 11, 8)

  #SPECIFICALLY TARGET CELL [X, 6] IN THE GTABLE, WHICH IS NORMALLY A SPACER AND SOMETIMES IS JUST NOTHING AND STICK THE NEW TITLE THERE. THIS WILL CLIP OVERTOP OF THE X AXIS LABELS FOR A TOP X AXIS, BUT THAT'S AN EDGE CASE THAT WOULDN'T BE DESIRABLE ANYHOW.
  idx = which(gt$layout$t <= target_row & gt$layout$b >= target_row &
                gt$layout$l <= 6 & gt$layout$r >= 6) #FIND ANY GROBS OCCUPYING CELL [X, 6].
  #IF THERE IS NO GROB THERE, OR IT'S JUST A SPACER OR THE BACKGROUND, WE CAN SAFELY MOVE THE Y AXIS TITLE TO THAT CELL.
  if (length(idx) == 0 || all(is.na(gt$layout$name[idx])) || all(gt$layout$name[idx] %in% c("spacer", "background"))) {
    gt = gtable::gtable_add_grob(
      gt,
      grob = grid::textGrob(lab,
                            x = 0,
                            y = 0.5,
                            hjust = 0, #NOT IDEAL FOR IT TO BE HARDCODED BUT W/E
                            vjust = ifelse(element$vjust, element$vjust, 0.5),
                            rot = 0, #THIS IS THE WHOLE POINT!
                            gp = element_to_gpar(element)), #TRANSLATE IN THEME CHARACTERISTICS TO THE GROB.
      t = target_row,
      l = 6,
      name = paste0("custom-", real_scale, "-title"),
      clip = "off"
    )
  }

  #MAKE ROW 8/11 HAVE A NON-ZERO HEIGHT (THIS IS THE ROW THAT SECOND GTABLE GROB WOULD NORMALLY GO IN--IT HAS A 0 HEIGHT UNLESS A TOP X-AXIS EXISTS.)
  gt$heights[target_row] = grid::unit(1.5, "lines") #=-FOR ME, 2 LINES SEEMS ENOUGH SPACE.

  #IF THE USER HAS FACETED, AND IF THEY HAVE STRIP LABELS AT THE TOP, AND THEY ARE TRYING TO PUT THE Y AXIS ON TOP INSTEAD OF ON BOTTOM (3 CHECKS!), THE FACET STRIP LABELS GO IN ROW 9 BY DEFAULT, BELOW THE NEW Y AXIS TITLE IN ROW 8, WHICH IS ILLOGICAL, SO WE FLIP THE ORDER OF THESE TWO ROWS.
  if (inherits(p$facet, "Facet") &&
      target_row == 8 &&
      any(!sapply(gt$grobs[which(gt$layout$t <= 9 &
                                 gt$layout$b >= 9 &
                                 grepl("strip-t", gt$layout$name))], function(x) {
                                   inherits(x, "zeroGrob")
                                 }))) {

    oldlayout = gt$layout #GET CURRENT LAYOUT AND HEIGHTS
    oldheights = gt$heights

    #GET OLD CONTENTS OF THESE TWO ROWS.
    oldrow8 = oldlayout$t == 8 & oldlayout$b == 8 #TRUE FOR CONTENTS ONLY IN ROW 8/9
    oldrow9 = oldlayout$t == 9 & oldlayout$b == 9

    #SWAPPING THE T AND B VALUES FOR ALL ELEMENTS IN THESE TWO ROWS.
    oldlayout$t[oldrow8] = 9
    oldlayout$b[oldrow8] = 9
    oldlayout$t[oldrow9] = 8
    oldlayout$b[oldrow9] = 8

    #SWAP THE HEIGHTS TOO.
    tmp = oldheights[8]
    oldheights[8] = oldheights[9]
    oldheights[9] = tmp

    #OVERWRITE GT WITH MODIFIED VERSIONS
    gt$layout = oldlayout
    gt$heights = oldheights
  }

  #WARNINGS REGIONS ------

  #WARNING #1--IF USERS HAVE MOVED THE X AXIS TO THE TOP OR HAVE DUPLICATED IT THERE, THE NEW Y AXIS TITLE WILL LIKELY CLIP THE LABELS
  axis_t_rows = which(grepl("^axis-t", gt$layout$name)) #FIND AXIS-T GROBS.
  axis_t_grobs = gt$grobs[axis_t_rows] #PULL THEM OUT

  #SEE IF ANY OF THESE GROBS FAIL TO INHERIT THE ZEROGROB CLASS, WHICH THEY GET IF THEY ARE ACTUALLY EMPTY.
  x_axis_top_visible = any(!vapply(axis_t_grobs, inherits, what = "zeroGrob", logical(1)))

  #IF ANY DO FAIL TO INHERIT, THEN WE WARN THE USER.
  if (x_axis_top_visible & location == "top") {
    warning("Heads-up: The top y axis title is likely to clip overtop of the x axis labels if your graph features a top x axis. Move the x axis to the bottom using \"position = 'top'\" in scale_x_*() (or remove the secondary x axis). Alternatively, set \"location = 'bottom'\" in switch_y_axis(). ", call. = FALSE)
  }

  # END WARNINGS ----

  grid::grid.draw(gt) #DRAW THE NEW GTABLE (PLOT) (HAS TO BE LAST!)

}
