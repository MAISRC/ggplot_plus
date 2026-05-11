# .onLoad() is a special function called automatically by R
# when the ggplotplus package is loaded via library().

#' geom_point_plus() shape registry
#'
#' Package-private environment used to store built-in and user-registered
#' point shapes for \code{geom_point_plus()}.
#'
#' @keywords internal
.onLoad = function(libname, pkgname) {
  S7::methods_register()
}

utils::globalVariables("ggplotplus_shapes_list")

.pointplus_shape_registry = new.env(parent = emptyenv()) #THIS CREATES A UNIFIED REGISTRY OF SHAPES, BOTH THOSE FROM THE PACKAGE AND THOSE ADDED BY THE USER, THAT GEOM_POINTS_PLUS() PULLS FROM.

#THIS INITIALIZES THE REGISTRY OF SHAPES UPON FIRST REQUEST INSTEAD ON ONLOAD.
.pointplus_shape_registry = new.env(parent = emptyenv())
.pointplus_shape_registry$.initialized = FALSE
.pointplus_shape_registry$.order = character(0)

.initialize_pointplus_shape_registry = function(force = FALSE) {

  current_shapes = setdiff(ls(.pointplus_shape_registry), c(".initialized", ".order"))

  if(!force &&
     isTRUE(.pointplus_shape_registry$.initialized) &&
     length(current_shapes) > 0) {
    return(invisible(NULL))
  }

  for(nm in names(ggplotplus_shapes_list)) {
    assign(nm, ggplotplus_shapes_list[[nm]], envir = .pointplus_shape_registry)
  }

  .pointplus_shape_registry$.order = names(ggplotplus_shapes_list)
  .pointplus_shape_registry$.initialized = TRUE

  invisible(NULL)
}
