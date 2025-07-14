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
