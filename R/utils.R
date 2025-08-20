#' @importFrom rlang %||%
#' @import ggplot2
#' @importFrom ggplot2 %+replace%
NULL


#CONVENIENCE FUNCTION--SYNTACTIC SUGAR FOR CHECKING WHETHER A VALUE EXISTS WITHIN A RANGE.
#' @keywords internal
is_between = function(low, high, value) {
  value >= low && value <= high
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
