#THIS FUNCTION ALLOWS US TO PLUG INTO THE USUAL GGPLOT SYNTAX BY ESSENTIALLY RAISING A FLAG THAT WE WANT TO TRIGGER THE AXIS_SWITCHER METHOD NEXT, WHICH KICKS OFF THE Y AXIS LABELING PROCESS.
yaxis_title_plus = function(location = "top") {
  structure(
    list(location = match.arg(location, c("top", "bottom"))),
    class = "axis_switcher"
  )
}

#THIS AMENDS GGPLOT_ADD'S NORMALLY BEHAVIOR TO FIND A PLOT PROVIDED AND APPEND THE CLASS "SWITCHER" TO IT.
ggplot_add.axis_switcher = function(object, plot, name = "switcher") {
  plot$y_axis_switch_location = object$location
  class(plot) = c("switcher", class(plot))
  plot + theme(axis.title.y = element_text(margin = margin(r = 0))) #ADD IN A THEME TO SWITCH HOW THE MARGIN IS ADJUSTED. 
}

#THIS AMENDS GGPLOT_BUILD'S NORMAL BEHAVIOR TO WATCH FOR A PLOT WITH THE "SWITCHER" CLASS AND, WHEN IT ENCOUNTERS IT, BUILDS IT NORMALLY, THEN APPENDS THE "SWITCHED" CLASS.
ggplot_build.switcher = function(plot) {
  class(plot) = c("gg", "ggplot")
  output = ggplot_build(plot)
  class(output) = c("switched", class(output))
  output
}

#THIS AMENDS GGPLOT_GTABLE'S NORMAL OPERATIONS SUCH THAT, IF A PLOT HAS THE "SWITCHED" CLASS, IT APPENDS A NEW Y AXIS TITLE USING OUR FUNCTION ABOVE.
ggplot_gtable.switched = function(plot) {
  loc = ifelse(!is.null(plot$plot$y_axis_switch_location), 
                plot$plot$y_axis_switch_location,
                "top")
  switch_axis_label(plot$plot, location = loc)
}

#A FUNCTION THAT CAN BE USED IN A TYPICAL + CHAIN TO CREATE A GGPLOT THAT HAS THE Y AXIS TITLE MOVED TO ABOVE THE PLOT, INTO A TEXTGROB THAT NORMALLY HOUSES TOP X AXIS INFO AND OTHERWISE HAS A HEIGHT OF 0. 
switch_axis_label = function(p, location = "top") {
  
  lab = p$scales$get_scales("y")$name #GET THE Y AXIS TITLE STRING PROVIDED TO ANY SCALE_Y_ FUNCTION FIRST.
  if(is.null(lab)) {
    lab = p$labels$y #OTHERWISE, GRAB THE DEFAULT Y LABEL FROM THE ORIGINAL DATA COLUMN'S NAME.
  }
  # 
  # #ALL THEME-RELATED ADJUSTMENTS MUST BE PORTED OVER MANUALLY. HERE, WE PORT OVER SIZE, TAKING EITHER A CUSTOM SIZE FROM THE PROVIDED THEME, IF ANY, OR ELSE THE SIZE FROM THE DEFAULT THEME. A SIMILAR MODEL COULD BE USED FOR CARRYING OVER THINGS LIKE FONT COLOR AND STYLE.

  #IF YOU'VE SPECIFIED A NEW Y SCALE TITLE VALUE VIA A SCALE_Y_ FUNCTION, THIS NUKES IT. 
  if(!is.null(p$scales$get_scales("y"))) {
    y = which(unlist(lapply(p$scales$scales, function(x) { "y" %in% x$aesthetics } )))
    p$scales$scales[[y]]$name = NULL
  }
  
  #THIS ALSO NUKES THE DEFAULT Y AXIS TITLE STRING.
  p = p + labs(y = NULL)
  
  #NOW, WE CONVERT THE GGPLOT WE ALREADY HAVE INTO A GTABLE.
  gt = p %>%  ggplot_build() %>% ggplot_gtable()
  
  #HERE, WE ATTEMPT TO PORT OVER ANY THEME-RELATED ADJUSTMENTS TO THE APPEARANCE OF THE Y AXIS TITLE.
  element = calc_element("axis.title.y", p$theme) #GRAB THE ELEMENT'S CURRENT THEME CHARACTERISTICS

  #NOT ALL THEME CHARACTERISTICS HAVE THE SAME NAME WITH GROBS, SO THIS TRANSLATES.
  translate_element = function(el) {
    el_list = as.list(el)
    # Translate key names
    if (!is.null(el_list$colour)){ el_list$col = el_list$colour }
    if (!is.null(el_list$face)){ el_list$fontface = el_list$face }
    if (!is.null(el_list$size)){ el_list$fontsize = el_list$size }
    
    return(el_list)
  }
  
  #THIS FUNCTION MATCHES UP GROB CHARACTERISTICS WITH THEME ONES WHERE APPROPRIATE.
  element_to_gpar = function(el) {
    el_list = translate_element(el)
    gpar_args = c("col", "fill", "alpha", "lty", "lwd", "lex", "lineend",
                   "linejoin", "linemitre", "fontsize", "cex", "fontfamily",
                   "fontface", "font", "lineheight")
    do.call(grid::gpar, el_list[intersect(names(el_list), gpar_args)])
  }

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
      name = "custom-y-title", 
      clip = "off"
    )
  }
  
  #MAKE ROW 8/11 HAVE A NON-ZERO HEIGHT (THIS IS THE ROW THAT SECOND GTABLE GROB WOULD NORMALLY GO IN--IT HAS A 0 HEIGHT UNLESS A TOP X-AXIS EXISTS.)
  gt$heights[target_row] = grid::unit(1.5, "lines") #=-FOR ME, 2 LINES SEEMS ENOUGH SPACE.
  #WARNINGS REGIONS ------
  
  #WARNING #1--IF USERS HAVE MOVED THE X AXIS TO THE TOP OR HAVE DUPLICATED IT THERE, THE NEW Y AXIS TITLE WILL LIKELY CLIP THE LABELS
  axis_t_rows = which(grepl("^axis-t", gt$layout$name)) #FIND AXIS-T GROBS.
  axis_t_grobs = gt$grobs[axis_t_rows] #PULL THEM OUT
  
  #SEE IF ANY OF THESE GROBS FAIL TO INHERIT THE ZEROGROB CLASS, WHICH THEY GET IF THEY ARE ACTUALLY EMPTY. 
  x_axis_top_visible = any(!vapply(axis_t_grobs, inherits, what = "zeroGrob", logical(1)))
  
  #IF ANY DO FAIL TO INHERIT, THEN WE WARN THE USER.
  if (x_axis_top_visible & location == "top") {
    warning("Heads-up: The top y axis title is likely to clip overtop of the x axis labels if your graph features a top x axis. Move the x axis to the bottom using \"position = 'top'\" in scale_x_*() (or remove the secondary x axis). Alternatively, set \"location = 'bottom'\" in switch_y_axis(). ")
  }
  
  #WARNING #2--IF USERS HAVE FACETED AND HAVE FACET STRIPS AT THE TOP POSITION, THE NEW Y AXIS TITLE WILL GO ABOVE RATHER THAN BELOW THEM. IN THIS CASE, I'D ADVISE MOVING THEM TO THE BOTTOM OF THE GRAPH INSTEAD.
  strip_t_rows = which(grepl("^strip-t", gt$layout$name)) #FIND ANY STRIP-T GROBS.
  strip_t_grobs = gt$grobs[strip_t_rows] #PULL THEM OUT
  
  #SEE IF ANY OF THESE GROBS FAIL TO INHERIT THE ZEROGROB CLASS, WHICH THEY GET IF THEY ARE ACTUALLY EMPTY. 
  top_strips = any(!vapply(strip_t_grobs, inherits, what = "zeroGrob", logical(1)))
  
  #IF ANY DO FAIL TO INHERIT, THEN WE WARN THE USER.
  if (top_strips) {
    warning("Heads-up: The top y axis title will be placed above any top strip labels on faceted graphs. This may not be ideal; in these instances, I recommend moving your top facet strips to the bottom. In facet_*(), specify \"switch = 'x'\" to do this. Alternatively, consider faceting by rows rather than columns to place strips on the sides.")
  }
  
  # END WARNINGS ----
  
  grid::grid.draw(gt) #DRAW THE NEW GTABLE (PLOT) (HAS TO BE LAST!)
  
}
