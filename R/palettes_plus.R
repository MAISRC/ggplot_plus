#' Make Viridis-Family Palettes the Ssession-Wide Defaults For Ggplots
#'
#' Sets session defaults for ggplot2’s colour/fill scales to viridis-family
#' palettes by overriding the `options()` keys that ggplot2 consults
#' when no explicit scale has been added. Discrete, continuous, and binned scales are
#' included. Users can still override them per-plot by adding any `scale_*_*()` call
#' as usual (last scale wins).
#'
#' You can specify viridis palettes either by **letter codes** `"A"`–`"H"`
#' (`"A" = "magma"`, `"B" = "inferno"`, `"C" = "plasma"`, `"D" = "viridis"`,
#' `"E" = "cividis"`, `"F" = "rocket"`, `"G" = "mako"`, `"H" = "turbo"`),
#' or by their **names**. Internally, this function registers small wrapper
#' functions under the ggplot2 option keys so that `...` is forwarded and user
#' overrides keep working.
#'
#' @param palette_discrete Viridis palette for **discrete** scales. Letter code
#'   `"A"`–`"H"` or name (e.g., `"viridis"`). Default `"D"`.
#' @param palette_continuous Viridis palette for **continuous/binned** scales.
#'   Letter code or name. Default `"E"` (`"cividis"`).
#' @param begin_discrete Low-end position for discrete palettes. Values within `[0, 1]`.
#'   Default `0`.
#' @param end_discrete High-end position for discrete palettes. Values within `[0, 1]`.
#'   Default `0.72`.
#' @param begin_continuous Low-end position for continuous/binned palettes.
#'   Default `0`.
#' @param end_continuous High-end position for continuous/binned palettes.
#'   Default `1`.
#'
#' @details
#' When you don’t add a colour/fill scale yourself, ggplot2 checks a few session
#' options to decide which scale to use. palettes_plus() sets those options to tiny
#' helper functions that return the viridis scales (e.g., scale_fill_viridis_c()).
#' Because they’re functions, ggplot2 can pass along any extra arguments via ....
#' And if you do add your own scale_*_*() in a plot, ggplot2 uses yours instead;
#' your custom code always wins.
#'
#' The following `options()` keys are set:
#' - `ggplot2.discrete.fill`, `ggplot2.discrete.colour`
#' - `ggplot2.continuous.fill`, `ggplot2.continuous.colour`
#' - `ggplot2.binned.fill`, `ggplot2.binned.colour`
#'
#' Each key is assigned a function that returns the appropriate
#' `scale_*_viridis_*()` with your `begin`, `end`, and `option` values. This is
#' exactly how ggplot2 looks up default scales when none are specified on a plot.
#'
#' This function is run automatically on package load. To **opt out**, set
#' `options(ggplot.plus.disable_palettes = TRUE)` before loading ggplot.plus.
#'
#' @return (Invisibly) `NULL`. The function has the side effect of setting
#'   session options.
#'
#' @examples
#' # Apply defaults for this session
#' palettes_plus()
#'
#' # Set different palettes and ranges if you prefer.
#' palettes_plus(palette_discrete = "plasma", palette_continuous = "viridis",
#'               begin_discrete = 0.1, end_discrete = 0.9)
#'
#' # Users can still override per-plot
#' library(ggplot2)
#' ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Petal.Length, colour = Species)) +
#'   ggplot2::geom_point() +
#'   ggplot2::scale_colour_brewer(palette = "Dark2")
#'
#' @seealso [ggplot2::scale_fill_viridis_d()], [ggplot2::scale_fill_viridis_c()],
#'   [ggplot2::scale_fill_viridis_b()], [ggplot2::scale_colour_viridis_d()],
#'   [ggplot2::scale_colour_viridis_c()], [ggplot2::scale_colour_viridis_b()],
#'   [ggplot2::theme_set()]
#' @export
palettes_plus = function(palette_discrete = "D",
                         palette_continuous = "E",
                         begin_discrete = 0,
                         end_discrete = 0.72,
                         begin_continuous = 0,
                         end_continuous = 1) {

  #THIS BIT ALLOWS USERS TO ENTER EITHER THE LETTERS OR THE NAMES AND EITHER WILL WORK
  .normalize_viridis = function(x) {
    map = c(A="magma", B="inferno", C="plasma", D="viridis",
            E="cividis", F="rocket", G="mako", H="turbo")
    if (length(x) == 1 && x %in% names(map)) map[[x]] else x
  }

  palette_discrete   = .normalize_viridis(palette_discrete)
  palette_continuous = .normalize_viridis(palette_continuous)

  #STASH THE PRE-SETTING OPTIONS FOR THESE KEYS, ASSUMING THERE ARE ANY.
  if(is.null(.ggplotplus_palette_env$old)) {
    .ggplotplus_palette_env$old = options()[.ggplotplus_palette_keys]
  }

options(
  ggplot2.discrete.fill = function(...) { ggplot2::scale_fill_viridis_d(begin = begin_discrete, end = end_discrete, option = palette_discrete,  ...) },
  ggplot2.discrete.colour = function(...) { ggplot2::scale_colour_viridis_d(begin = begin_discrete, end = end_discrete, option = palette_discrete, ...) },
  ggplot2.continuous.fill = function(...) { ggplot2::scale_fill_viridis_c(begin = begin_continuous, end = end_continuous, option = palette_continuous, ...) },
  ggplot2.continuous.colour = function(...) { ggplot2::scale_colour_viridis_c(begin = begin_continuous, end = end_continuous, option = palette_continuous, ...) },
  ggplot2.binned.fill   = function(...) { ggplot2::scale_fill_viridis_b(begin = begin_continuous, end = end_continuous, option = palette_continuous, ...) },
  ggplot2.binned.colour = function(...) { ggplot2::scale_colour_viridis_b(begin = begin_continuous, end = end_continuous, option = palette_continuous, ...) }
)
invisible(NULL) #<--ENSURES A RETURN BUT ONE THAT'S SILENT.
}

#' Restore Ggplot2 Palette Defaults (Undo `palettes_plus()`)
#'
#' Restores the ggplot2 palette-related `options()` keys to whatever they were
#' **before** `palettes_plus()` was called. Called automatically
#' on package detach/unload but can also be called manually.
#'
#' @return (Invisibly) `NULL`.
#'
#' @examples
#' palettes_plus()
#' # ... later ...
#' palettes_reset()
#'
#' @seealso [palettes_plus()]
#' @export
palettes_reset = function() {
  old = .ggplotplus_palette_env$old
  if(!is.null(old)) { options(old) } #RETURN TO ANY PREVIOUS GLOBAL OPTIONS WE CAN FIND. ELSE DO NOTHING.
  invisible(NULL)
}
