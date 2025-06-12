gridlines_plus = function(color = "gray90", 
                   linewidth = 1.2, 
                   linetype = "solid") {
  structure(
    list(color = color, linewidth = linewidth, linetype = linetype),
    class = "gridlines_plus"
  )
}

ggplot_add.gridlines_plus = function(object, plot, name) {
  #STEP 1: GET THE MAPPING FOR THE PROVIDED PLOT.
  mapping = plot$mapping
  
  #STEP 2: GET CLASSES OF VARIABLES FOR X AND Y AXES.
  x_var = rlang::as_name(mapping$x)
  y_var = rlang::as_name(mapping$y)
  
  #STEP 3: DETERMINE IF NUMERIC.
  x_is_cont = is.numeric(plot$data[[x_var]])
  y_is_cont = is.numeric(plot$data[[y_var]])
  
  #BASELINE NEW THEME CALL.
  grid_theme = theme()
  
  #IF X IS CONTINUOUS, ADD JUST MAJOR X AXIS GRIDLINES IN THIS DIRECTION.
  if (x_is_cont) {
    grid_theme = grid_theme + theme(
      panel.grid.major.x = element_line(
        color = object$color,
        linewidth = object$linewidth,
        linetype = object$linetype
      )
    )
  }
  #SAME FOR Y
  if (y_is_cont) {
    grid_theme = grid_theme + theme(
      panel.grid.major.y = element_line(
        color = object$color,
        linewidth = object$linewidth,
        linetype = object$linetype
      )
    )
  }
  #DRAW THE PLOT WITH THE NEW GRIDLINES.
  plot + grid_theme
}