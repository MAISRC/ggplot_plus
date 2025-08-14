#' @importFrom rlang %||%
#' @import ggplot2
#' @importFrom ggplot2 %+replace%
NULL

##INTERNAL HELPER TO ENSURE I'M ALWAYS CALLING GGPLOT'S OG ggplot_build METHOD IN CERTAIN CIRCUMSTANCES. WILL NEED TO WATCH OUT FOR A GGPLOT2 REFACTOR.
ggbuild_ggplot = getFromNamespace("ggplot_build.ggplot", "ggplot2")

#CONVENIENCE FUNCTION--SYNTACTIC SUGAR FOR CHECKING WHETHER A VALUE EXISTS WITHIN A RANGE.
#' @keywords internal
is_between = function(low, high, value) {
  value >= low && value <= high
}

#' Convert Between a Couple of Different Ways of Referencing the Same Aesthetic
#'
#' This is an internal convenience function that allows translation of the names of aesthetics like "colour" into ones like "col" used by `grid`'s functions.
#'
#' @keywords internal
translate_element = function(el) {
  el_list = as.list(el)
  #TRANSLATE BETWEEN SOME DIFFERENT AESTHETICS DEPENDING ON HOW EXACTLY THEY ARE CODED.
  if (!is.null(el_list$colour)){ el_list$col = el_list$colour }
  if (!is.null(el_list$face)){ el_list$fontface = el_list$face }
  if (!is.null(el_list$size)){ el_list$fontsize = el_list$size }

  return(el_list)
}


#' Translate Between Ggplot's and Gpar's Attribute Names.
#'
#' This is an internal convenience function that matches up `ggplot2`'s aesthetics names with those expected by the `grid::gpar` function so that user-specified aesthetics get properly carried over into the final product.
#' @keywords internal
element_to_gpar = function(el) {
  el_list = translate_element(el)
  #ATTRIBUTES THAT GPARS NEED TO HAVE--LINE THESE UP WITH THE ATTRIBUTES OF THE ELEMENT BEING PORTED IN.
  gpar_args = c("col", "fill", "alpha", "lty", "lwd", "lex", "lineend",
                "linejoin", "linemitre", "fontsize", "cex", "fontfamily",
                "fontface", "font", "lineheight")
  do.call(grid::gpar, el_list[intersect(names(el_list), gpar_args)])
}

#' Remove Constants From a List of Provided and Inherited Aesthetics
#'
#' This is an internal convenience function that takes a list of mapped aesthetics and checks to see if any of them are actually constants (e.g., they were provided inside of aes() but are not mapping to a variable). If it finds anything, it removes them from mapping and instead ports them to a "target_list" and triggers a warning.
#' @keywords internal
extract_mapped_constants = function(aes_obj, target_list, silence = FALSE, location = "mapping") {
  mapped_constants = c()

  for (nm in names(aes_obj)) {
    expr = rlang::get_expr(aes_obj[[nm]])
    if (is.atomic(expr) && length(expr) == 1) {
      # Move constant to target list
      target_list[[nm]] = expr
      mapped_constants = c(mapped_constants, nm)
    }
  }

  # Remove from aes_obj
  aes_obj = aes_obj[!names(aes_obj) %in% mapped_constants]

  # Optionally warn
  if (length(mapped_constants) > 0 && !silence) {
    warning(sprintf(
      "You set the following aesthetic(s) to constants inside of an aes() call inside your %s call: %s. This is not recommended practice; set constants outside aes(), e.g., `fill = 'black'`, not `aes(fill = 'black')`, for more predictable behavior.",
      location, paste(mapped_constants, collapse = ", ")
    ), call. = FALSE)
  }

  list(aes = aes_obj, constants = target_list)
}
