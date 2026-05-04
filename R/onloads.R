# .onLoad() is a special function called automatically by R
# when the ggplotplus package is loaded via library().

.onLoad = function(libname, pkgname) {
  S7::methods_register()
}
