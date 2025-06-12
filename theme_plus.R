theme_plus = function(...) {
    
  user_args = list(...)
  structure(list(user_args = user_args),
            class = "theme_plus")
}

ggplot_add.theme_plus = function(object, plot, name) {
  
  user_args = object$user_args #EXTRACT THE USER'S ARGUMENTS

  #DEFINE THE DEFAULT THEME HERE.
  default_theme = theme(
  # aspect.ratio = 1, #I WANT TO DO MORE RESEARCH ON WHAT THIS VALUE SHOULD BE OR HOW TO CALCULATE IT.
  axis.line = element_line(color = "black", linewidth = 1.2), #ADD THICK BLACK X AND Y AXIS LINES
  axis.title.x = element_text(color = "black", size = 18, margin = margin(t = 10)), #ADD TOP MARGIN TO X AXIS TITLE.
  axis.title.y = element_text(color = "black", size = 18, vjust = 0.25, margin = margin(r  = 15)),
  axis.text = element_text(size = 16, color = "black"), #ENSURE AXIS LABELS ARE BLACK AND SIZE 16
  axis.ticks.length = unit(0.3, "cm"), #INCREASE SIZE OF AXIS TICK MARKS TO BE MORE NOTICEABLE. 
  legend.position = "top",
  legend.direction = "horizontal",
  legend.title = element_text(color = "black", size = 18, margin = margin(r = 15)),
  legend.box.just = "bottom",
  legend.key.spacing.x = unit(0.5, "cm"),
  legend.text = element_text(size = 16, color = "black"),
  legend.justification = "right",
  legend.key = element_rect(fill = "transparent", color = "white"),
  legend.background = element_rect(color = "white", fill = "white"), 
  legend.ticks.length = unit(0.3, "cm"), 
  legend.frame = element_rect(color = "black", linewidth = 1.2), 
  panel.border = element_blank(), 
  panel.grid = element_blank(), #ELIMINATE MAJOR AND MINOR GRIDLINES
  panel.background = element_rect(fill = "white"), #SWITCH FROM GRAY TO WHITE BACKGROUND
  panel.spacing = unit(1, "cm"),
  plot.title = element_blank(),
  plot.subtitle = element_blank(), 
  strip.background = element_rect(color = "white", fill = "white"), 
  strip.text = element_text(color = "black", size = 16, face = "bold"),
  strip.text.y = element_text(angle = 0)
) 
  
  #ADD THE THEMES TO THE PLOT, WITH THE USER'S PROVIDED SPECIFICATIONS SECOND SO THEY OVERRIDE ANY RELEVANT DEFAULTS. 
  plot + default_theme + do.call(theme, user_args)
}