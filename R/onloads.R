# .onLoad() is a special function called automatically by R
# when the ggplot.plus package is loaded via library().
#
# This function sets the default discrete and continuous
# ggplot2 color/fill scales using palettes_plus(), so users
# do not need to call it manually and it's on by default,
# though users can call it again if they want to change anything.
#
# These options override:
#   - ggplot2.discrete.fill
#   - ggplot2.discrete.colour
#   - ggplot2.continuous.fill
#   - ggplot2.continuous.colour
#
.onLoad = function(libname, pkgname) {
  #THIS CHECKS A GLOBAL SETTING FIRST AND DOES NOTHING IF THAT SETTING IS TRUE BECAUSE THAT WOULD MEAN IT'S ALREADY SET.
  if(isTRUE(getOption("ggplot.plus.disable_palettes", FALSE))) { return(invisible()) }
  palettes_plus()
}

#TURNS THESE OVERRIDES BACK OFF WHEN THE PACKAGE IS DETACHED OR UNLOADED (E.G. AT SESSION END)
.onDetach = function(libpath) {
  palettes_reset()
}

.onUnload = function(libpath) {
  palettes_reset()
}
