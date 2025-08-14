#' Internal: Legend-Position Theme Conditional Logic
#'
#' Builds a small theme fragment for customizing `"top"` vs `"right"` legends.
#'
#' @inheritParams theme_plus
#' @return A ggplot2 theme object.
#' @keywords internal
#' @noRd
.determine_legend_theme = function(legend_pos = c("top", "right")) {
  legend_pos = match.arg(legend_pos) #CONFIRMS WE GOT ONLY ONE OR THE OTHER VALID OPTION.

  if(legend_pos == "top") {
    ggplot2::theme(
      legend.key.width = grid::unit(1.5, "cm"),
      legend.key.height = grid::unit(0.75, "cm"),
      legend.title = ggplot2::element_text(margin = ggplot2::margin(r = 15), vjust = 0.5),
      legend.box.just = "bottom",
      legend.justification = "right",
      legend.key.spacing.x = grid::unit(0.5, "cm"),
      legend.position = "top",
      legend.direction = "horizontal"
    )
  } else {
    ggplot2::theme(
      legend.key.height = grid::unit(1.5, "cm"),
      legend.key.width = grid::unit(0.75, "cm"),
      legend.title = ggplot2::element_text(margin = ggplot2::margin(b = 15), hjust = 0.5),
      legend.box.just = "right",
      legend.justification = "right",
      legend.key.spacing.y = grid::unit(0.5, "cm"),
      legend.position = "right",
      legend.direction = "vertical")
  }
}



#' A Universal Design-Oriented Base Ggplot2 Theme With Scalable and Overridable Defaults
#'
#' `theme_plus()` returns a complete ggplot2 theme designed to make
#' publication-quality, accessible graphs easier to produce. It keeps all of
#' ggplot2’s normal behaviors (last theme wins; user overrides take precedence),
#' but bakes in opinionated defaults with Universal Design in mind. A few knobs
#' let you scale typography/lines, flip the legend layout, and switch the
#' background color if desired.
#'
#' Internally, text sizes are expressed with `rel()`, so they scale with
#' `base_font_size`. Line/rect line thicknesses start from `base_linewidth` and
#' `base_rectlinewidth` and scale similarly with `rel()`. The function builds
#' a complete base theme using `%+replace%`, then *adds* any user overrides via
#' `theme(...)`, so the user's preferences always take precedence.
#'
#' @param ... Optional additional theme settings passed to [ggplot2::theme()].
#'   These are applied *after* the base theme, so the theme's defaults
#'   (same as in ggplot2).
#' @param legend_pos Where to put the legend. `"top"` (default) creates a
#'   horizontal stripe at the top; `"right"` uses a vertical legend at the
#'   right (ggplot2’s usual position) but with design modifications.
#' @param base_font_size Base text size (in points) for most text elements. These
#'   will scale via `rel()`. Default is `16`.
#' @param base_linewidth Baseline thickness for most **line** theme elements
#'   (e.g., axis lines, tick marks). Defaults to `1.2`. Specific elements
#'    may use `rel()` multipliers on top of this.
#' @param base_rectlinewidth Baseline line thickness for most **rect** theme elements
#'   (e.g., legend frames). Defaults to `1.2`.
#' @param line_color Default color for most line elements (axis lines, frames, etc.).
#'   Defaults to `"black"`.
#' @param text_color Default color for most text elements. Defaults to `"black"`.
#' @param background_color Background fill applied to the panel, plot, legend,
#'   and strip backgrounds. Defaults to a slightly warm white, `"#FFFEFD"`.
#'
#' @return A ggplot2 theme object to add with `+`.
#'
#' @examples
#' # Basic use
#' library(ggplot2)
#' ggplot(iris, aes(Sepal.Length, Petal.Length, colour = Species)) +
#'   geom_point() +
#'   theme_plus()
#'
#' # Prefer the right-side legend and pure white background
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_plus(legend_pos = "right", background_color = "white")
#'
#' # Scale text up and make lines a bit lighter
#' ggplot(iris, aes(Sepal.Length, Petal.Length)) +
#'   geom_point() +
#'   theme_plus(base_font_size = 18, base_linewidth = 1.0)
#'
#' # You can still override any element normally via theme()
#' ggplot(iris, aes(Sepal.Length, Petal.Length)) +
#'   geom_point() +
#'   theme_plus() +
#'   theme(axis.line = element_line(linewidth = 0.8))
#'
#' # But you could just as easily do so via theme_plus()
#' ggplot(iris, aes(Sepal.Length, Petal.Length)) +
#'   geom_point() +
#'   theme_plus(axis.line = element_line(linewidth = 0.8))
#'
#' @seealso [ggplot2::theme()], [ggplot2::theme_gray()], [ggplot2::theme_get()]
#' @export
theme_plus = function(...,
                      legend_pos = c("top", "right"),
                      base_font_size = 16,
                      base_linewidth = 1.2,
                      base_rectlinewidth = 1.2,
                      line_color = "black",
                      text_color = "black",
                      background_color = "#FFFEFD") {

  legend_pos = match.arg(legend_pos)

  #WE NOW USE THE REL() FUNCTION TO KEEP THINGS SCALED RELATIVE TO THE BASE_FONT_SIZE FOR CONVENIENCE

  default_theme = ggplot2::theme_gray(base_size = base_font_size,
                                      base_line_size = base_linewidth,
                                      base_rect_size = base_rectlinewidth
  ) %+replace% #<--SPECIAL GGPLOT2 OPERATOR FOR THEMES
    ggplot2::theme(
      # aspect.ratio = 1, #I WANT TO DO MORE RESEARCH ON WHAT THIS VALUE SHOULD BE OR HOW TO CALCULATE IT.
      axis.line = ggplot2::element_line(color = line_color, linewidth = ggplot2::rel(1), lineend = "square"), #ADD THICK BLACK X AND Y AXIS LINES WITH SQUARE ENDS TO ENSURE THAT THEY APPEAR TO VISUALLY MEET.
      axis.title.x = ggplot2::element_text(color = text_color, size = ggplot2::rel(1.125), margin = ggplot2::margin(t = 10)), #ADD TOP MARGIN TO X AXIS TITLE.
      axis.title.y = ggplot2::element_text(color = text_color, size = ggplot2::rel(1.125), vjust = 0.5, margin = ggplot2::margin(r = 15), angle = 90),
      axis.text = ggplot2::element_text(size = ggplot2::rel(1), color = text_color), #ENSURE AXIS LABELS ARE BLACK AND SIZE 16
      axis.ticks.length = grid::unit(0.3, "cm"), #INCREASE SIZE OF AXIS TICK MARKS TO BE MORE NOTICEABLE.
      axis.ticks = element_line(color = line_color, linewidth = ggplot2::rel(0.75)),

      legend.title = ggplot2::element_text(color = text_color, size = ggplot2::rel(1.125)),
      legend.text = ggplot2::element_text(size = ggplot2::rel(1), color = text_color),
      legend.key = ggplot2::element_rect(fill = "transparent", color = "white"),
      legend.background = ggplot2::element_rect(color = NA, fill = background_color),
      legend.ticks.length = grid::unit(0.2, "cm"),
      legend.frame = ggplot2::element_rect(color = line_color, linewidth = ggplot2::rel(1)), #MAKE SOLID BLACK LINES FOR THE LEGEND BORDER FOR CONTINUOUS SCALES.
      legend.ticks = ggplot2::element_line(color = "white", linewidth = ggplot2::rel(0.75), linetype = "solid"), #MAKE THE TICKS WHITE

      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(), #ELIMINATE MAJOR AND MINOR GRIDLINES
      panel.background = ggplot2::element_rect(fill = background_color, color = NA), #SWITCH FROM GRAY TO WHITE BACKGROUND
      panel.spacing = grid::unit(1, "cm"),

      plot.background = ggplot2::element_rect(fill = background_color, color = NA),
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),

      strip.background = ggplot2::element_rect(color = NA, fill = background_color),
      strip.text = ggplot2::element_text(color = text_color, size = ggplot2::rel(1), face = "bold"),
      strip.text.y = ggplot2::element_text(margin = ggplot2::margin(l=5), angle = 0),
      strip.text.x = ggplot2::element_text(margin = ggplot2::margin(b=5)),
      strip.placement = "outside", #THIS ALWAYS ENSURES THAT AXIS LABELS GO CLOSER TO THE AXIS THAN THE STRIP LABELS WOULD.

      complete = TRUE #<--KEY INPUT, ENSURES FEWER SURPRISES IN HOW THIS THEME BEHAVES VIS-A-VIS A DEFAULT THEME. COMPLETE THEMES ARE TREATED AS A COLLECTION OF FALLBACK VALUES FOR WHEN A USER DOESN'T SPECIFY SOMETHING OR DOESN'T INHERIT SOMETHING FROM A MORE GLOBAL VALUE.
    )

  #THIS NEXT LINE ENSURES THAT WE JUST RETURN A THEME OPTION RIGHT AWAY SO GGPLOT2 CAN HANDLE ALL THE COLLISIONS AND ADDING AS IT NORMALLY WOULD. NO NEED FOR A GGPLOT.ADD DISPATCH!
  #WE COLLIDE WITH A BASE GGPLOT2 THEME LAST IN CASE THE USER PROVIDES ANY CUSTOM GGPLOT2 THEMING OF THEIR OWN HERE, AS A CONVENIENCE.
  default_theme %+replace% .determine_legend_theme(legend_pos) + ggplot2::theme(...)
}


#' Get The Ggplot.plus Default Theme For Setting Or Modifying
#'
#' Convenience function that just returns [theme_plus()] with the given
#' settings applied. It’s handy when you want to inspect or reuse the package’s
#' opinionated defaults, or set them globally with [ggplot2::theme_set()]. This
#' function does *not* look at the currently active theme; for that, see
#' [ggplot2::theme_get()].
#'
#' @inheritParams theme_plus
#'
#' @return A ggplot2 theme object (the result of `theme_plus(...)`).
#'
#' @examples
#' library(ggplot2)
#'
#' # Use it like any theme object
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   theme_plus_get_default()
#'
#' # Prefer the right-side legend and a white background
#' ggplot(iris, aes(Sepal.Length, Petal.Length, colour = Species)) +
#'   geom_point() +
#'   theme_plus_get_default(legend_pos = "right", background_color = "white")
#'
#' # Make ggplot.plus defaults the session-wide theme
#' theme_set(theme_plus_get_default())
#'
#' @seealso [theme_plus()], [ggplot2::theme_get()], [ggplot2::theme_set()]
#' @export
theme_plus_get_default = function(legend_pos = c("top", "right"),
                                  base_font_size = 16,
                                  base_linewidth = 1.2,
                                  base_rectlinewidth = 1.2,
                                  line_color = "black",
                                  text_color = "black",
                                  background_color = "#FFFEFD") {
  legend_pos = match.arg(legend_pos)

  theme_plus(legend_pos = legend_pos,
             base_font_size = base_font_size,
             base_linewidth = base_linewidth,
             base_rectlinewidth = base_rectlinewidth,
             line_color = line_color,
             text_color = text_color,
             background_color = background_color)
}



