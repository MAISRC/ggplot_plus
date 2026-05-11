#' Subtle, Minimal Gridlines For When and Where They Help
#'
#' Adds light and easily ignored major gridlines along only axes mapped to
#' continuous variables. Minor gridlines are
#' blanked. This enables the benefits of gridlines (in instances where there are some) but minimizes visual clutter and cognitive load.
#'
#' `gridlines_plus()` ignores any theme instructions to blank major panel scales in either the x or y direction via, e.g., `theme_plus(panel.grid.major.y = ggplot2::element_blank())`. Instead, users can set, e.g., noty == TRUE to prevent gridlines from being drawn in a specific direction, even if that scale is continuous. Similarly, gridline color, linewidth, and linetypes should be set directly in `gridlines_plus()` instead of via `theme()`. Trying to do the latter will fail without error or warning!
#'
#' @param color Gridline color. Single character string. Default: `"gray90"`.
#' @param linewidth Gridline width (theme line units). Single numeric. Default: `1.2`.
#' @param linetype Gridline type. Single string (e.g., `"solid"`, `"dashed"`).
#' @param notx,noty Logicals indicating whether gridlines should not be drawn in a specific direction (i.e., the user wants those to be blank even when the axis is continuous). Default to FALSE (gridlines are added).
#'
#' @return An ggplot class object for adding to a plot with `+`.
#'
#' @details
#' Under the hood, `gridlines_plus()` checks layer and/or global mappings to
#' see if `x` and/or `y` are continuous. It does this by inspecting the trained panel scales.
#' It then turns on **major** gridlines for continuous directions and explicitly
#' blanks gridlines for other axes (as well as **all minor** gridlines).
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Petal.Length)) +
#'   ggplot2::geom_point() +
#'   theme_plus() + #WE DON'T RECOMMEND USING gridlines_plus() WITHOUT ALSO USING theme_plus()
#'   gridlines_plus()
#'
#' # Only y is continuous here (x is discrete) → y-only major gridlines
#' ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) +
#'   ggplot2::geom_boxplot() +
#'   gridlines_plus(color = "grey85", linewidth = 1, linetype = "dashed")
#'
#' # Works with derived continuous axes (histogram)
#' ggplot2::ggplot(mtcars, aes(mpg)) +
#'   ggplot2::geom_histogram() +
#'   gridlines_plus()
#'
#' @export
gridlines_plus = function(color = "gray90",
                          linewidth = 1.2,
                          linetype = "solid",
                          notx = FALSE,
                          noty = FALSE) {
  GridlinesPlus(
    color = color,
    linewidth = linewidth,
    linetype = linetype,
    notx = notx,
    noty = noty
  )
}


#' Relocate a Y Axis Title to Above the Y Axis on a Ggplot and Turn it Horizontal.
#'
#' This function relocates the y axis title of a ggplot to the top, above the y axis line and left-justified to the left edge of the y axis labels, sort of like a plot subtitle. It also orients the text horizontally for space-efficiency and easy reading. This is otherwise difficult to do using `ggplot2`'s default styling tools.
#'
#' @param location A length-1 character string matching either "top" or "bottom" for the placement of the new y axis title. Defaults to `"top"`. `"bottom"` should generally only be used when the x axis labels have been moved to the top of the graph (uncommon).
#' @param nudgeTopLegendDown A length-1 logical indicating whether a top legend (box) (if any) should be moved down to align with the relocated y axis title (where they could clip into each other). Defaults to FALSE.
#'
#' @param nudgeHowMuch A length-1 positive integer indicating how much to nudge the top legend (box) (if any) down, if `nudgeTopLegendDown` == `TRUE`. Defaults to `20` points as a general guess and may need adjusting.
#'
#' @return An ggplot class object for adding to a plot with `+`.
#' @examples
#' #WE DO NOT RECOMMEND USING yaxis_title_plus() WITHOUT theme_plus()
#' ggplot2::ggplot(iris, ggplot2::aes(x=Sepal.Length, y=Petal.Length)) +
#' ggplot2::geom_point() +
#' theme_plus() +
#' yaxis_title_plus()
#' @export
yaxis_title_plus = function(location = "top",
                            nudgeTopLegendDown = FALSE,
                            nudgeHowMuch = 20) {
  YAxisTitlePlus(
    location = location,
    nudgeTopLegendDown = nudgeTopLegendDown,
    nudgeHowMuch = nudgeHowMuch
  )
}



#' Continuous Scales With Endpoint-Aware Breaks
#'
#' `scale_continuous_plus()` is an opinionated wrapper around ggplot2's
#' continuous x, y, colour, and fill scales. It chooses "pretty" breaks while
#' gently expanding the scale limits so breaks generally will appear near both
#' ends of the data range.
#'
#' This is useful because ggplot2's default continuous scales frequently will leave the ends of an
#' axis or colorbar visually unlabeled, making it look as if an endpoint break is
#' missing.
#'
#' @param scale Character string specifying which scale to modify. Options are
#'   `"x"`, `"y"`, `"colour"`/`"color"`, and `"fill"`.
#' @param ... Additional arguments passed to the corresponding ggplot2
#'   continuous scale function. Arguments such as `name`, `labels`, `guide`,
#'   `position`, and `expand` may be supplied. User-supplied `breaks`, `limits`,
#'   `n.breaks`, `trans`, and `transform` are ignored with a warning because
#'   this function controls those components directly.
#' @param thin.labels Logical. If `TRUE`, every other break label is blanked
#'   to reduce crowding. Defaults to `FALSE`.
#' @param pad.labels Character string, either `"start"` or `"end"`. Used when a
#'   user-supplied label vector is shorter than the internally computed break
#'   vector by one label (which must be assessed via trial and error currently). `"start"` pads a blank label at the beginning;
#'   `"end"` pads one at the end.
#' @param target.breaks Integer target number of major breaks. This is a target and
#'   not a guarantee because breaks are chosen using a "pretty" break algorithm.
#'   Default is `5`.
#' @param buffer_frac Numeric fraction of the data span used to decide whether a
#'   break is close enough to each endpoint. Default is `0.05`.
#' @param split_name Logical. If `TRUE`, spaces in a named `name` argument are
#'   replaced with line breaks. This can help long axis or legend titles fit
#'   better. Default is `FALSE`.
#'
#' @details
#' `scale_continuous_plus()` routes to one of
#' [ggplot2::scale_x_continuous()], [ggplot2::scale_y_continuous()],
#' [ggplot2::scale_colour_continuous()], or
#' [ggplot2::scale_fill_continuous()] based on `scale`.
#'
#' Unlike the ggplot2 defaults, this function intentionally controls `breaks`
#' and `limits`. If either is supplied through `...`, it's ignored with a
#' warning. Transformed scales are also not currently supported; pre-transform
#' the data or use ggplot2's scale functions directly when a transformed scale is
#' needed.
#'
#' User-supplied label vectors are supported, but endpoint-aware breaks can
#' sometimes create hidden outer breaks. When possible, this function pads label
#' vectors with blank labels to align them with the computed break vector in length. If
#' alignment is ambiguous by one label, use `pad.labels` to choose which side of the input labels vector to
#' pad.
#'
#' @return A ggplot2 continuous scale object.
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
#'   geom_point() +
#'   scale_continuous_plus(scale = "x") +
#'   scale_continuous_plus(scale = "y")
#'
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, fill = Petal.Length)) +
#'   geom_point(shape = 21) +
#'   scale_continuous_plus(scale = "x") +
#'   scale_continuous_plus(scale = "y") +
#'   scale_continuous_plus(scale = "fill")
#'
#' ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
#'   geom_point() +
#'   scale_continuous_plus(
#'     scale = "y",
#'     name = "Sepal length",
#'     labels = LETTERS[1:5],
#'     pad.labels = "start"
#'   )
#'
#' @export
scale_continuous_plus =
  function(scale = NA,
           ...,
           thin.labels = FALSE,
           pad.labels = "start",
           target.breaks = 5,
           buffer_frac = 0.05,
           split_name = FALSE) {

  lookup = data.frame(
    incoming = c("fill", "color", "colour", "x", "y"),
    outgoing = c("scale_fill_continuous",
                 "scale_colour_continuous",
                 "scale_colour_continuous",
                 "scale_x_continuous",
                 "scale_y_continuous"
                )
  )

  if(is.na(scale)) { stop("A scale is required. Please provide one. The options are \"x\", \"y\", \"colour\", and \"fill\"") }

  user.args = list(...)

  any_user_breaks = .partial_match_user_arg(user.args, "breaks")
  any_user_limits = .partial_match_user_arg(user.args, "limits")
  bad = !is.null(any_user_breaks) | !is.null(any_user_limits)

  if(bad) {
    warning("This function sets opinionated breaks and limits, so yours were ignored. If you want to set these yourself, use ggplot2::scale_*_continuous().",
         call. = FALSE)
   user.args = .remove_partial_match_user_arg(user.args, "breaks")
   user.args = .remove_partial_match_user_arg(user.args, "limits")
  }

  any_nbreaks = .partial_match_user_arg(user.args, "n.breaks")

  if(!is.null(any_nbreaks)) {
    warning("Use the target.breaks argument instead of the n.breaks argument to control the target number of breaks.")
    user.args = .remove_partial_match_user_arg(user.args, "n.breaks")
  }

  any_trans = .partial_match_user_arg(user.args, "trans")

  if(!is.null(any_trans)) {
    warning(
      "A `trans` argument was supplied, but that argument is deprecated, so it was ignored.",
      call. = FALSE
    )
    user.args = .remove_partial_match_user_arg(user.args, "trans")
  }

  any_transform = .partial_match_user_arg(user.args, "transform")

  if(!is.null(any_transform)) {
  warning(
    "A `transform` argument was supplied, but `scale_continuous_plus()` does not currently support transformed scales, so your transform input was ignored. Pre-transform the data or use `ggplot2::scale_*_continuous(transform = ...)` instead.",
    call. = FALSE
  )
    user.args = .remove_partial_match_user_arg(user.args, "transform")
  }

  split_requested = isTRUE(split_name)
  any_name = .partial_match_user_arg(user.args, "name")
  if(split_requested && is.null(any_name)) {
    warning("`split_name` was set to TRUE but no named `name` argument was provided. Please provide a named `name` argument to use `split_name.")
    split_name = FALSE
  }

  if(split_requested &&
     !is.null(any_name)) {
    user.args = .remove_partial_match_user_arg(user.args, "name")
    user.args$name = gsub(" ", "\n", any_name)
  }

  args = user.args

  if(is.null(.partial_match_user_arg(args, "expand"))) {
    args$expand = c(0,0)
  }

  args$breaks = function(x) {
   .endpoint_breaks(x, n = target.breaks, buffer_frac = buffer_frac, Return = "breaks")
  }

  args$limits = function(x) {
    .endpoint_breaks(x, n = target.breaks, buffer_frac = buffer_frac, Return = "limits")
   }

  #MAKE USER LABEL DECISIONS IF ANY USER LABELS EXIST
  any_user_labels = .partial_match_user_arg(user.args, "labels")

  if (!is.null(any_user_labels) || thin.labels) {

  #DETERMINE WHAT KIND OF USER LABELS WERE GIVEN AND BRANCH
  user_label_mode =
    if (is.null(any_user_labels)) {
      "none"
    } else if (is.function(any_user_labels)) {
      "function"
    } else if (is.atomic(any_user_labels) || is.expression(any_user_labels)) {
      "vector"
    } else {
      stop("`labels` must be NULL, a function, or a vector/expression.", call. = FALSE)
    }

  args$labels = function(x) {

    labs = x #DEFAULT TO X, OVERRIDE AS NEEDED IF USER PROVIDED LABELS.

    if(user_label_mode != "none") {
    if(user_label_mode == "function") {
        labs = any_user_labels(x) #RUN USER LABELING FUNCTION
      } else {

        #OTHERWISE, USER PROVIDED VECTOR, SO LET'S ADDRESS THE 4 POSSIBILITIES:
        delta = length(x) - length(any_user_labels)

      if (delta == 0) { #1: THEY MATCH PERFECTLY, SO ROLL WITH THEM.
        labs = any_user_labels

      } else if (delta == 1) { #2: THEY MISS BY ONE, UNFORTUNATELY, SO EITHER PAD LEFT OR PAD RIGHT AS USER REQUESTS.
        if (pad.labels == "start") {
          labs = c("", any_user_labels)
        } else {
          labs = c(any_user_labels, "")
        }
      } else if (delta == 2) { ##3: THEY MISS BY TWO, SO PAD EITHER SIDE.
        labs = c("", any_user_labels, "")
      } else {
        stop( #4: GIVE UP AND CRY THAT THE LABELS DON'T MATCH.
          paste0( #THIS IS A NICE ERROR CHATGPT--TELLS YOU HOW MANY YOU SHOULD HAVE BEEN GOING FOR.
            "Custom labels could not be aligned to the computed breaks. ",
            "Expected ", length(x), " labels but received ", length(any_user_labels), "."
          ),
          call. = FALSE
        )
      }

      }
    }

    #IF THE USER WANTS EVERY OTHER REAL LABEL BLANKED...
    if(thin.labels) {

      #A LITTLE HARD TO GUESS WHAT TO DO IF THEY DIDN'T SPECIFY LABELS SINCE WE COULD BE OFF, BUT WE'LL JUST GUESS AND THEY CAN USE PAD.LABELS TO NUDGE IT.
      if(user_label_mode == "none") {
        if(pad.labels == "start") {
          labs[seq(from = 1, to = length(labs), by = 2)] = ""
        } else {
          labs[seq(from = 2, to = length(labs), by = 2)] = ""
        }
      } else {

      labs[is.na(labs)] = "" #GUARDS AGAINST GGPLOT TRAINING NONSENSE.
      first_real = which(labs != "")[1] #FIND THE FIRST "REAL" LABEL
      if(!is.na(first_real)) {
        blank_these = seq(from = first_real + 1,
                  to = length(labs),
                  by = 2)
        labs[blank_these] = ""
      }
     }
    }

    return(labs)
   }
  } #END LABEL SETTING SKIP

  do.call(lookup$outgoing[lookup$incoming == scale], args)
}


#' A Universal Design-Oriented Base Ggplot2 Theme With Scalable and Overridable Defaults
#'
#' `theme_plus()` returns a ggplot2 theme designed to make
#' publication-quality, accessible graphs easier to produce. It keeps all of
#' ggplot2’s normal behaviors (last theme wins; user overrides take precedence),
#' but bakes in opinionated defaults with Universal Design in mind. A few knobs
#' let you scale typography/lines, flip the legend layout, and switch the
#' background color if desired.
#'
#' Internally, text sizes are expressed with `rel()`, so they scale with
#' `base_font_size`. Line/rect line thicknesses start from `base_linewidth` and
#' `base_rectlinewidth` and scale similarly with `rel()`.
#'
#' The function builds
#' a  base theme, then *adds* any user overrides via
#' `theme(...)`, so the user's preferences always take precedence.
#'
#' @param ... Optional additional theme settings passed to [ggplot2::theme()]. These are applied *after* the base theme, so the theme's defaults only "win" when no matching settings are provided by the user
#'   (same as in ggplot2).
#' @param legend_pos Where to put the legend. `"top"` (default) creates a
#'   horizontal stripe at the top for the legend (box) when one is present; `"right"` uses a vertical legend at the
#'   right (ggplot2’s usual position) but with design modifications.
#' @param base_font_size Base text size (in points) for most text elements. These
#'   will scale via `rel()`. Default is `16`.
#' @param base_linewidth Baseline thickness for most **line** theme elements
#'   (e.g., axis lines and tick marks). Defaults to `1.2`. Specific elements
#'    may use `rel()` multipliers on top of this.
#' @param base_rectlinewidth Baseline line thickness for most **rect** theme elements
#'   (e.g., legend frames). Defaults to `1.2`.
#' @param line_color Default color for most line elements (axis lines, frames, etc.).
#'   Defaults to `"black"`.
#' @param text_color Default color for most text elements. Defaults to `"black"`.
#' @param background_color Background fill applied to the panel, plot, legend,
#'   and strip backgrounds. Defaults to a slightly warm white, `"#FFFEFD"`, to reduce eyestrain.
#' @param palette_discrete,palette_continuous Default viridis-family color palette codes ("A" through "H") to use for discrete and continuous scales, respectively.
#' @param begin_discrete,end_discrete,begin_continuous,end_continuous Numeric values ranging between 0 and 1 for where to begin drawing colors from a viridis palette for a discrete and continuous color scale, respectively.
#' @param export_width,export_height Length-1 numeric values indicating your intended export (most likely via ggplot2::ggsave()) width and height, respectively. This rescales font and line sizes internally to stay relatively appropriately for your intended export size.
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
                      legend_pos = "top",
                      base_font_size = 16,
                      base_linewidth = 1.2,
                      base_rectlinewidth = 1.2,
                      line_color = "black",
                      text_color = "black",
                      background_color = "#FFFEFD",
                      palette_discrete = "D",
                      palette_continuous = "E",
                      begin_discrete = 0,
                      end_discrete = 0.72,
                      begin_continuous = 0,
                      end_continuous = 1,
                      export_width = 7.25,
                      export_height = 5.95) {

  #ONE ODD INTERACTION IS WITH strip.text(), WHICH CAN ONLY BE BLANKED BY HITTING X AND Y SEPARATELY. SOMETHING TO PONDER WHEN ADJUSTING SUBSCALES SEPARATELY...
  user_theme = ggplot2::theme(...)
  dots = rlang::list2(...)
  if(inherits(dots$strip.text, "element_blank")) {
    user_theme = user_theme +
      ggplot2::theme(
        strip.text.x = ggplot2::element_blank(),
        strip.text.y = ggplot2::element_blank()
      )
  }


  gg_palette_theme = .theme_plus_palettes(palette_discrete,
                                          palette_continuous,
                                          begin_discrete,
                                          end_discrete,
                                          begin_continuous,
                                          end_continuous)

  ref_width = 6.25
  ref_height = 7.79

  scale_factor = sqrt((export_width * export_height) / (ref_width * ref_height))

  base_font_size = base_font_size * scale_factor
  base_linewidth = base_linewidth * scale_factor
  base_rectlinewidth = base_rectlinewidth * scale_factor

  #WE NOW USE THE REL() FUNCTION TO KEEP THINGS SCALED RELATIVE TO THE BASE_FONT_SIZE FOR CONVENIENCE

  default_theme = ggplot2::theme_gray(base_size = base_font_size,
                                      base_line_size = base_linewidth,
                                      base_rect_size = base_rectlinewidth
  ) +
    ggplot2::theme_sub_axis(
      line = ggplot2::element_line(color = line_color, linewidth = ggplot2::rel(1), lineend = "square"), #ADD THICK BLACK X AND Y AXIS LINES WITH SQUARE ENDS TO ENSURE THAT THEY APPEAR TO VISUALLY MEET.
      title = ggplot2::element_text(color = text_color, size = ggplot2::rel(1.125)),
      text = ggplot2::element_text(size = ggplot2::rel(1), color = text_color), #ENSURE AXIS LABELS ARE BLACK AND SIZE 16
      ticks.length = ggplot2::unit(0.25, "cm"), #INCREASE SIZE OF AXIS TICK MARKS TO BE MORE NOTICEABLE.
      ticks = ggplot2::element_line(color = line_color, linewidth = ggplot2::rel(0.75)),
    ) +
    ggplot2::theme_sub_axis_x(title = ggplot2::element_text(margin = ggplot2::margin(t = 10))) +
    ggplot2::theme_sub_axis_y(title = ggplot2::element_text(vjust = 0.5, margin = ggplot2::margin(r = 15), angle = 90)) +
    ggplot2::theme_sub_legend(
      box = "vertical", #<--MAKES MULTIPLE LEGENDS GO VERTICAL
      title = ggplot2::element_text(color = text_color, size = ggplot2::rel(1.125)),
      text = ggplot2::element_text(size = ggplot2::rel(1), color = text_color),
      key = ggplot2::element_rect(fill = "transparent", color = "white"),
      background = ggplot2::element_rect(color = NA, fill = background_color),
      ticks.length = ggplot2::unit(0.2, "cm"),
      ticks = ggplot2::element_line(color = "white", linewidth = ggplot2::rel(0.75), linetype = "solid"), #MAKE THE TICKS WHITE
      frame = ggplot2::element_rect(color = line_color, linewidth = ggplot2::rel(1)), #MAKE SOLID BLACK LINES FOR THE LEGEND BORDER FOR CONTINUOUS SCALES.
    ) +
    ggplot2::theme_sub_panel(
      border = ggplot2::element_blank(),
      grid = ggplot2::element_blank(), #ELIMINATE MAJOR AND MINOR GRIDLINES
      background = ggplot2::element_rect(fill = background_color, color = NA), #SWITCH FROM GRAY TO WHITE BACKGROUND
      spacing = ggplot2::unit(1, "cm"),
    ) +
    ggplot2::theme_sub_plot(
      background = ggplot2::element_rect(fill = background_color, color = NA),
      title = ggplot2::element_blank(),
      subtitle = ggplot2::element_blank(),
    ) +
    ggplot2::theme_sub_strip(
      background = ggplot2::element_rect(color = NA, fill = background_color),
      text = ggplot2::element_text(color = text_color, size = ggplot2::rel(1), face = "bold"),
      text.y = ggplot2::element_text(margin = ggplot2::margin(l=5), angle = 0),
      text.x = ggplot2::element_text(margin = ggplot2::margin(b=5)),
      placement = "outside", #THIS ALWAYS ENSURES THAT AXIS LABELS GO CLOSER TO THE AXIS THAN THE STRIP LABELS WOULD.
    ) +
    ggplot2::theme(
      geom = ggplot2::element_geom( #IN HERE IS WHERE WE CAN NOW ADD GENERAL GEOM_*-RELATED STYLE OPINIONS.
        pointsize = 5,
        pointshape = 21,
        borderwidth = 1.2,
        colour = "black",
        linetype = "solid",
        linewidth = 1.35
      ),
    )

  #THIS NEXT LINE ENSURES THAT WE JUST RETURN A THEME OPTION RIGHT AWAY SO GGPLOT2 CAN HANDLE ALL THE COLLISIONS AND ADDING AS IT NORMALLY WOULD. NO NEED FOR A GGPLOT.ADD DISPATCH!
  #WE COLLIDE WITH A BASE GGPLOT2 THEME LAST IN CASE THE USER PROVIDES ANY CUSTOM GGPLOT2 THEMING OF THEIR OWN HERE, AS A CONVENIENCE.
  theme_plus2add = default_theme + .determine_legend_theme(legend_pos) + gg_palette_theme + user_theme

  ThemePlus(
    applyGeomDefaults = TRUE,
    theme2add = theme_plus2add
  )

}



#' Create and add a scatterplot layer to your `ggplot2` graph with new, distinctive shapes.
#'
#' This function behaves similarly to `ggplot2::geom_point()` except that it takes several new inputs: `shapes`, `n_shapes`, `shape_values`, `legend_title`, `key_size`, and `show_shape_scale`. These are explained below.
#'
#' Collectively, these inputs allow `geom_point_plus()` to access and draw several new and distinctive shapes that are designed to be more readily distinguishable from one another when shape communicates difference.
#'
#' To see the special shapes available via this function run `geom_point_plus_shapes()`.
#'
#' Note: As of Version 0.5.2, shapes 21-25 in R's default shapes palette are now also available via `geom_point_plus_shapes()`; these are called "circle", "square", "diamond", "triangle_up", and "triangle_down", respectively, though they can also be referred to by number.
#'
#' @param mapping Set of aesthetic mappings created by aes(), as in `ggplot2::geom_point()`.
#' @param data The data to be displayed in this layer, as in `ggplot2::geom_point()`.
#' @param stat The statistical transformation to use on the data for this layer, as in `ggplot2::geom_point()`.
#' @param position A position adjustment to use on the data for this layer, as in `ggplot2::geom_point()`.
#' @param avail_shapes A named list of custom shapes to be drawn in place of `ggplot2`'s standard palette of shapes. Defaults to `NULL` and is replaced internally with the palette of shapes designed specifically for use in `geom_point_plus()`. This should probably not be changed unless users have created new shapes they would like to use instead.
#' @param n_shapes A length-1 integer corresponding to the number of distinct shapes the function is allowed to pull from the shapes palette specified to `avail_shapes`. Defaults to the length of `avail_shapes` and should probably not be changed.
#' @param chosen_shapes A character string referring by name to elements in the current shapes registry that the function should use to allocate shapes to values, e.g. `c("flower", "octagon", "squircle)`. These are provided internally to a `scale_shape_manual()` call and are meant to circumvent the need for such a call to specify a specific subset of shapes to be used from the new shapes palette. Defaults to `NULL`, i.e., shapes are pulled from `shapes.list` in order. Numerical values will use `ggplot2`'s default shapes instead.
#' @param legend_title A length-1 character string corresponding to the name to be used for the shape legend title (if any). This is passed internally to `scale_shape_manual()` and is meant to help circumvent the need for the user to specify any such call directly.
#' @param key_size A length-1 numeric value corresponding to the desired size of the legend keys. Defaults to 8. This is passed internally to `scale_shape_manual()` and is meant to help circumvent the need for the user to specify any such call directly.
#' @param include_shape_legend Logical indicating whether a shape legend will be shown (one is always shown unless this is set to FALSE, even when shape is being mapped to a constant and thus a legend may not be appropriate).
#' @param ... Other arguments passed on to this layer()'s params argument, as in `ggplot2::geom_point()`.
#' @param na.rm Logical value controlling whether missing values should be removed from the data with a warning or silently, as in `ggplot2::geom_point()`.
#' @param show.legend Logical value controlling whether this layer should be included in the legend(s), as in `ggplot2::geom_point()`.
#' @param inherit.aes Logical controlling whether global aesthetics specified in `ggplot2::ggplot()` should be inherited locally by this layer or not, as in `ggplot2::geom_point()`.
#' @param show_shape_scale Logical controlling whether a call to `ggplot2::shape_scale_manual()` should be included as part of the function's operations. Generally, this should be set to `TRUE` unless shape is being mapped to a constant, in which case leaving this `TRUE` would trigger a legend that is redundant.
#' @return A ggplot2 layer object.
#' @examples
#' ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, fill = drat)) +
#' geom_point_plus(ggplot2::aes(shape = factor(gear)), size = 5)
#' ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, fill = factor(cyl))) +
#' geom_point_plus(ggplot2::aes(shape = factor(carb)),
#' shape_values = c("squircle", "lotus", "sunburst", "octagon", "cross", "oval"),
#' size = 5, stroke = 0.4)
#' ggplot2::ggplot(iris, ggplot2::aes(Petal.Width, Petal.Length, fill = Species)) +
#' geom_point_plus(ggplot2::aes(shape = Species), size = 5, alpha = 0.7)
#'
#' @export
geom_point_plus = function(mapping = NULL,
                           data = NULL,
                           stat = "identity",
                           position = "identity",
                           avail_shapes = NULL, #A NAMED LIST OF SHAPES. DEFAULTS TO THOSE PROVIDED BY ggplotplus PLUS THOSE ADDED BY THE USER VIA ADD_SHAPE_PLUS. BUT NOT EVALUATED HERE--EVALUATED INSIDE FUNCTION ENVIRON.
                           n_shapes = length(avail_shapes), #HOW MANY DISTINCT SHAPES SHOULD BE PULLED FROM THE AVAILABLE PALETTE? DEFAULTS TO ALL OF THEM.
                           chosen_shapes = NULL, #WE PROVIDE DIRECT ACCESS TO THE VALUES ARGUMENT OF SCALE_SHAPE_MANUAL VIA THIS PARAMETER. THIS WAY, A USER NEEDN'T TACK ON AN ADDITIONAL CALL TO SCALE_SHAPE_MANUAL() TO CUSTOMIZE THE SHAPES USED.
                           legend_title = NULL, #WE ALSO PROVIDE DIRECT ACCESS TO THE TITLE ARGUMENT OF THE LEGEND, AS CHANGING THIS MANUALLY WOULD OTHERWISE REQUIRE ANOTHER CALL TO SCALE_SHAPE_DISCRETE AND THAT WOULD TRIGGER A WARNING AND RESET TO THE SHAPES PALETTE GGPLOT2 GENERALLY USES.
                           key_size = 8, #WE PROVIDE DIRECT ACCESS TO THE SIZES OF THE KEYS IN THE LEGEND TOO.
                           include_shape_legend = TRUE, #WE PROVIDE DIRECT ACCESS TO WHETHER OR NOT A SHAPE LEGEND GETS SHOWN, FOR USE IN SINGLE-SHAPE SCATTERPLOTS WHERE THE CUSTOM SHAPES ARE USED INSTEAD OF GGPLOT2 DEFAULTS.
                           ...,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           show_shape_scale = TRUE) {

  #EVALUATES THE SHAPES REGISTRY HERE, UNLESS A DIFFERENT ONE IS PROVIDED.
  if(is.null(avail_shapes)) {
    avail_shapes = .pointplus_shapes()
  }

  dot_args = rlang::list2(...)

  #IF SHAPE IS NOT BEING MAPPED, JUST RETURN THE LAYER WITHOUT DRAWING ANY SCALE
  #THIS ONLY CHECKS MAPPING, WHICH IS LOCAL. IF SHAPE IS MAPPED GLOBALLY INSTEAD, A SCALE_SHAPE_MANUAL CALL IS PRETTY HARMLESS.
  shape_is_mapped = .has_mapped_aes(mapping, dot_args, "shape")

  chosen_shapes = .standardize_pointplus_shape_names(chosen_shapes) #SEE MIDDLEWARE FOR THIS HELPER.

  #IF CHOSEN_SHAPES IS A CONSTANT, PASS IT ALONG AS A CONSTANT AND NOT AN AES.
  if(!shape_is_mapped &&
     !is.null(chosen_shapes) &&
     length(chosen_shapes) == 1 &&
     is.character(chosen_shapes) &&
     chosen_shapes %in% names(avail_shapes)) {
    dot_args$shape = chosen_shapes
    chosen_shapes = NULL
  }

  #THIS FUNCTION IS MOSTLY JUST A WRAPPER TO GEOM_POINT2 INTERNALLY.
  geom_call = do.call(
    geom_point2,
    c(
      list(
        mapping = mapping,
        data = data,
        stat = stat,
        position = position,
        shapes = avail_shapes,
        na.rm = na.rm,
        show.legend = if(isTRUE(include_shape_legend)) {show.legend}  else {c(shape = FALSE)},
        inherit.aes = inherit.aes
      ),
      dot_args
    )
  )

  #HERE, SELECT THE EXACT SHAPES TO DRAW FROM THE SHAPES PALETTE, DEFAULTING TO THE SHAPES IN THE REGISTRY IN ORDER IF NO SPECIFIC SHAPES WERE CHOSEN.
  if(is.null(chosen_shapes) || length(chosen_shapes) == 0) {
    values = names(avail_shapes)[seq_len(min(length(avail_shapes), n_shapes))]
  } else {
    values = chosen_shapes
  }

  #BUILD THE LEGEND IF WE'RE GOING TO, BUT ONLY PUT IN TITLE IF THE USER PROVIDED ONE.
  guide_obj = if(isTRUE(include_shape_legend)) {
    ggplot2::guide_legend(override.aes = list(size = key_size))
  } else {
    "none"
  }

  #ADD THE SHAPE SCALE
  scale_call = if(!is.null(legend_title)) {
    ggplot2::scale_shape_manual(name = legend_title, values = values, guide = guide_obj)
  } else {
    ggplot2::scale_shape_manual(values = values, guide = guide_obj)
  }

  #AND ADD A SIZE SCALE TO MAKE THE DEFAULT SIZES A BIT HEFTIER.
  if(is.null(.partial_match_user_arg(dot_args, "size")) &&
     isTRUE(any(c("size") %in% names(mapping)))) {
    size_scale_call = ggplot2::scale_size(range = c(3, 5.5))
  } else {
    size_scale_call = NULL
  }

  #IF THE USER HAS MAPPED SHAPE GLOBALLY + DOES NOT WANT A SCALE_SHAPE_MANUAL CALL (BECAUSE THEY'VE MAYBE SET SHAPE TO A CONSTANT THERE), THIS WILL PREVENT THE SCALE FROM SHOWING BY SUPPRESSING THE SCALE CALL IF THEY'VE TOGGLED THIS.
  if(show_shape_scale == FALSE) {
    return(geom_call)
  }

  #BE MORE EXPLICIT ABOUT THE OUTPUT STRUCTURE.
  Filter(Negate(is.null), list(geom_call, scale_call, size_scale_call))

}


#' Jittered points with ggplotplus point shapes
#'
#' \code{geom_jitter_plus()} is a convenience wrapper around
#' \code{geom_point_plus()} that applies jittering to reduce overplotting.
#' It supports the same custom shape palette and fillable point rendering
#' as \code{geom_point_plus()} while exposing the familiar \code{width},
#' \code{height}, and \code{seed} arguments used by
#' \code{ggplot2::position_jitter()}.
#'
#' @inheritParams ggplot2::geom_point
#' @param width,height Amount of horizontal and vertical jitter. Passed to
#'   \code{ggplot2::position_jitter()} when \code{position = "jitter"}.
#' @param seed Random seed used by \code{ggplot2::position_jitter()} to make
#'   jittering reproducible. Defaults to \code{NA}, matching `ggplot2`.
#' @param ... Additional arguments passed to \code{geom_point_plus()}, including
#'   ggplotplus-specific arguments such as \code{chosen_shapes} and
#'   \code{legend_title}. See `?geom_point_plus` for details.
#'
#' @return A ggplot2 layer.
#'
#' @examples
#' ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
#'   geom_jitter_plus(
#'     ggplot2::aes(shape = Species, fill = Petal.Length),
#'     width = 0.15,
#'     seed = 1,
#'     colour = "black"
#'   )
#'
#' ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
#'   geom_jitter_plus(
#'     ggplot2::aes(shape = Species),
#'     chosen_shapes = c("plus", "flower", "lotus"),
#'     seed = 123
#'   )
#'
#' @export
geom_jitter_plus = function(mapping = NULL,
                            data = NULL,
                            stat = "identity",
                            position = "jitter",
                            ...,
                            width = NULL,
                            height = NULL,
                            seed = NA,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {

  #SANITIZING ANY PARTIAL MATCHING INPUTS.
  dot.args = list(...)
  anywidth = .partial_match_user_arg(args = dot.args, target = "width")
  if(length(anywidth) > 0) {
    width = anywidth
    dot.args = .remove_partial_match_user_arg(dot.args, "width")
  }
  anyheight = .partial_match_user_arg(args = dot.args, target = "height")
  if(length(anyheight) > 0) {
    width = anyheight
    dot.args = .remove_partial_match_user_arg(dot.args, "height")
  }
  anyseed = .partial_match_user_arg(args = dot.args, target = "seed")
  if(length(anyseed) > 0) {
    width = anyseed
    dot.args = .remove_partial_match_user_arg(dot.args, "seed")
  }

  #IF WIDTH OR HEIGHT IS NOT NULL, AND/OR SEED IS NOT NULL, AND WE'RE ON POSITION JITTER...
  if(!is.null(width) || !is.null(height) || !is.na(seed)) {
    if(!identical(position, "jitter")) {
      warning(
        "`width`, `height`, and `seed` are ignored when `position` is not \"jitter\".",
        call. = FALSE
      )
    } else { #...PASS TO GGPLOT'S POSITION_JITTER.
      position = ggplot2::position_jitter(
        width = width,
        height = height,
        seed = seed
      )
    }
  }

  #FROM THERE ON OUT, IT'S ALL GEOM_POINT_PLUS'S JOB.
  geom_point_plus(
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    ...,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes
  )
}


#' Demo plot showing geom_point_plus() shapes
#'
#' A prebuilt ggplot object displaying the nine custom point shapes designed specifically for use in `geom_point_plus()`. As of Version 0.5.2, other shapes are also available, but those are not shown via this function. See the documentation for `geom_point_plus()` for details.
#'
#' @format A ggplot object.
#'
#' @export
geom_point_plus_shapes = function() {

  ggplot2::ggplot(data = data.frame(x = rep(c(0.5,1.5,2.5), each = 3),
                                                           y = rep(c(1,2,3), times = 3),
                                                           shape = factor(1:9))) +
  geom_point_plus(ggplot2::aes(x = .data$x, y = .data$y, shape = .data$shape, fill = .data$shape), chosen_shapes = c("squircle", "octagon", "flower", "economy", "plus", "waffle", "oval", "sunburst", "lotus"),
                  size = 10, stroke = 1)+
  ggplot2::theme_minimal() +
  ggplot2::lims(y=c(0.5, 3.5), x = c(0.4, 3)) +
  ggplot2::annotate("text", x = 0.8, y = 1, label = "Closed\nrounded\nuncrossed", size = 5) +
  ggplot2::annotate("text", x = 0.8, y = 2, label = "Closed\npointed\nuncrossed", size = 5) +
  ggplot2::annotate("text", x = 0.8, y = 3, label = "Closed\nrounded\ncrossed", size = 5) +
  ggplot2::annotate("text", x = 1.8, y = 1, label = "Open\npointed\nuncrossed", size = 5) +
  ggplot2::annotate("text", x = 1.82, y = 2, label = "Intermediate", size = 5) +
  ggplot2::annotate("text", x = 1.8, y = 3, label = "Closed\npointed\ncrossed", size = 5) +
  ggplot2::annotate("text", x = 2.8, y = 1, label = "Open\nrounded\nuncrossed", size = 5) +
  ggplot2::annotate("text", x = 2.8, y = 2, label = "Open\npointed\ncrossed", size = 5) +
  ggplot2::annotate("text", x = 2.8, y = 3, label = "Open\nrounded\ncrossed", size = 5) +
  ggplot2::theme(legend.position = "none",
                 axis.text = ggplot2::element_blank(),
                 axis.title = ggplot2::element_blank(),
                 axis.line = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_blank(),
                 panel.grid = ggplot2::element_blank()) +
  ggplot2::annotate("text", x = 0.5, y = 0.75, label = "squircle", size = 5, fontface = 'bold') +
  ggplot2::annotate("text", x = 0.5, y = 1.75, label = "octagon", size = 5, fontface = 'bold') +
  ggplot2::annotate("text", x = 0.5, y = 2.75, label = "flower", size = 5, fontface = 'bold') +
  ggplot2::annotate("text", x = 1.5, y = 0.75, label = "economy", size = 5, fontface = 'bold') +
  ggplot2::annotate("text", x = 1.5, y = 1.75, label = "plus", size = 5, fontface = 'bold') +
  ggplot2::annotate("text", x = 1.5, y = 2.75, label = "waffle", size = 5, fontface = 'bold') +
  ggplot2::annotate("text", x = 2.5, y = 0.75, label = "oval", size = 5, fontface = 'bold') +
  ggplot2::annotate("text", x = 2.5, y = 1.75, label = "sunburst", size = 5, fontface = 'bold') +
  ggplot2::annotate("text", x = 2.5, y = 2.75, label = "lotus", size = 5, fontface = 'bold')
}


#' Add a custom shape for geom_point_plus()
#'
#' Registers a custom point shape for use with \code{geom_point_plus()}.
#' Custom shapes are defined by a data frame of connected polygon coordinates
#' and, once validated, are stored in a session-level shape registry.
#'
#' A shape must be supplied as a data frame with columns \code{x}, \code{y},
#' and \code{piece}. The \code{x} and \code{y} columns define the vertices of
#' the shape, centered around \code{0, 0}. Coordinates should generally be
#' scaled to about \code{+/-0.4} to match the scaling of built-in point shapes.
#' The \code{piece} column identifies separate polygon pieces within the same
#' shape to generate "holes," as appropriate.
#'
#' For inspiration and to model inputs, see `ggplotplus_shapes_list` for the
#' structure of the built-in point shapes.
#'
#' @param name A single character string giving the name of the new shape.
#' @param shape A data frame with columns \code{x}, \code{y}, and \code{piece}.
#' @param overwrite Logical. If \code{FALSE}, the default, an error is returned
#'   when \code{name} already exists in the shape registry. If \code{TRUE}, the
#'   existing shape is replaced with the newly provided shape. Useful if, e.g.,
#'   you'd like to scale an existing shape up or down in size.
#' @param ... Additional arguments. Partial argument matching is supported for
#' friendly UX.
#'
#' @return Invisibly returns the registered shape name.
#'
#' @examples
#' test_star = data.frame(
#'   x = c(0.000,  0.118,  0.380,  0.190,  0.235,
#'         0.000, -0.235, -0.190, -0.380, -0.118),
#'   y = c(0.400,  0.124,  0.124, -0.047, -0.324,
#'        -0.153, -0.324, -0.047,  0.124,  0.124),
#'   piece = 1
#' )
#'
#' add_shape_plus("test_star", test_star)
#'
#' @export
add_shape_plus = function(name = NULL,
                                    shape = NULL,
                                    overwrite = FALSE,
                          ...) {

  #TRY TO PARTIAL MATCH NAME FIRST.
  dot.args = list(...)
  if(length(dot.args) > 0) {
  any_name = .partial_match_user_arg(dot.args, "name")
  if(length(any_name) > 0) {
    name = any_name
    dot.args = .remove_partial_match_user_arg(dot.args, "name")
    }

  #IF UNNAMED FIRST ARG COULD REASONABLY BE NAME, PUT IN THAT FORMAL NAME.
  if(is.character(dot.args[[1]]) &&
     length(dot.args[[1]]) == 1) {
    name = dot.args[[1]]
    dot.args[[1]] = NULL
   }

  }

  #SAFETY CHECK--DO WE HAVE A SINGLE CHARACTER NAME THAT ISN'T BLANK?
  if(!is.character(name) || length(name) != 1 || is.na(name) || name == "") {
    stop("`name` must be a single non-missing character string. Give your new shape a name!", call. = FALSE)
  }


  name = trimws(name)
  name = .standardize_pointplus_shape_names(name) #PREVENTS A USER FROM ADDING A "21" OR SOMETHING.

  if(name == ".initialized") {
    stop("`.initialized` is reserved for internal ggplotplus use. Do not call shapes by that name.", call. = FALSE)
  }

  .initialize_pointplus_shape_registry()

  #HANDLES SITUATION WHERE A USER TRIES TO ADD A SHAPE WE'VE ALREADY GOT.
  if(exists(name, envir = .pointplus_shape_registry, inherits = FALSE) && !overwrite) {
    stop(
      "A pointplus shape named `", name, "` already exists. ",
      "Use `overwrite = TRUE` to replace it.",
      call. = FALSE
    )
  }


  #NOW, DO THE SAME CLEANING ON SHAPE
  if(length(dot.args) > 0) {
    any_shape = .partial_match_user_arg(dot.args, "shape")
    if(length(any_shape) > 0) {
      shape = any_shape
      dot.args = .remove_partial_match_user_arg(dot.args, "shape")
    }

    #IF UNNAMED FIRST REMAINING ARG COULD REASONABLY BE SHAPE, PUT IN THAT FORMAL NAME.
    if(is.data.frame(dot.args[[1]]) &&
       ncol(dot.args[[1]]) >= 3 &&
       all(c("x", "y", "piece") %in% names(dot.args[[1]]))) { #<--SHOULD BE THREE COLS W/ THESE NAMES AT LEAST.
      shape = dot.args[[1]]
      dot.args[[1]] = NULL
    }

  }

  shape = .validate_pointplus_shape(shape, name = name) #NOW ACTUALLY TRY TO USE THE SHAPE THEY'VE GIVEN.

  assign(name, shape, envir = .pointplus_shape_registry) #IF WE GET THIS FAR, ADD THE SHAPE TO THE REGISTRY.

  #MAKE SURE TO PRESERVE THE INTENDED ORDER AND APPEND USER-ADDED SHAPES TO THE END.
  if(!name %in% .pointplus_shape_registry$.order) {
    .pointplus_shape_registry$.order = c(.pointplus_shape_registry$.order, name)
  }

  if(length(dot.args) > 0) {
    dot.args = NULL
    warning("Additional, ambiguous arguments were provided but not used. ")
  }

  invisible(name) #AN INVISIBLE RETURN FOR SAFETY.
}
