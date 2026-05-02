# .onLoad() is a special function called automatically by R
# when the ggplot_plus package is loaded via library().

.onLoad = function(libname, pkgname) {
  S7::methods_register()
}
