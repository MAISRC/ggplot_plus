# IMPORT COMMANDS ---------------------------------------------------------

#' @importFrom rlang %||% list2 .data
#' @import ggplot2
#' @importFrom ggplot2 update_ggplot class_ggplot ggplot_build ggplot_gtable
#' @importFrom polyclip polyclip
#' @importFrom scales pretty_breaks pal_viridis
#' @importFrom viridisLite viridis
NULL


# CUSTOM OBJECTS ----------------------------------------------------------

#' Custom shape palette for `geom_point_plus()`
#'
#' A named list of custom point shapes used by `geom_point_plus()`. Each element
#' contains `x` and `y` coordinates plus a `piece` identifier used when drawing
#' filled shapes and holes.
#'
#' @format A named list of data frames. Each data frame has columns:
#' \describe{
#'   \item{x}{X coordinates for the shape outline.}
#'   \item{y}{Y coordinates for the shape outline.}
#'   \item{piece}{Integer identifier for separate polygon pieces or holes.}
#' }
#'
#' @examples
#' names(ggplotplus_shapes_list)
"ggplotplus_shapes_list"

# geom_plus_defaults ------------------------------------------------------
#' Default settings for geometry layers created by `geom_plus()`
#'
#' A named list of default aesthetics used by `geom_plus()` to control styling of the resulting geometry layers.
#'
#' Most geom-specific default aesthetics are set in `theme_plus()`; what is set here is what can't be set there.
#'
#' @format A named list with elements like "point", "jitter", "boxplot", etc., corresponding to commonly used ggplot2 geometries. Use names(geom_plus_defaults) for a full list.
#' @keywords internal
#' @noRd
geom_plus_defaults = list(
  point = list(
    aes = list(fill = NA),
    params = list()
  ),
  point_plus = list(
    aes = list(fill = NA),
    params = list()
  ),
  jitter = list(
    aes = list(fill = NA),
    params = list()
  ),
  count = list(
    aes = list(fill = NA),
    params = list()
  ),
  boxplot = list(
    aes = list(),
    params = list(
      staplewidth = 0.25,
      outlier.colour = "black",
      outlier.stroke = 1.2,
      outlier.fill = "transparent",
      outlier.shape = 21
    )
  ),
  violin = list(
    aes = list(fill = "white"),
    params = list(
      geom_params = list(
        quantile_gp = list(
      linetype = "solid",
      linewidth = 0.5,
      colour = "black"
        )
    ),
    stat_params = list(
      quantiles = c(0.25, 0.5, 0.75)
    )
   )
  ),
  bar = list(
    aes = list(fill = "transparent"),
    params = list()
  ),
  col = list(
    aes = list(fill = "transparent"),
    params = list()
  ),
  histogram = list(
    aes = list(fill = "transparent"),
    params = list()
  ),
  line = list(
    aes = list(alpha = 1),
    params = list()
  ),
  freqpoly = list(
    aes = list(alpha = 1),
    params = list()
  ),
  segment = list(
    aes = list(alpha = 1),
    params = list()
  ),
  abline = list(
    aes = list(alpha = 1),
    params = list()
  ),
  hline = list(
    aes = list(alpha = 1),
    params = list()
  ),
  vline = list(
    aes = list(alpha = 1),
    params = list()
  ),
  curve = list(
    aes = list(alpha = 1),
    params = list()
  ),
  smooth = list(
    aes = list(fill = "black", alpha = 0.3),
    params = list()
  ),
  area = list(
    aes = list(fill = "black", alpha = 0.3),
    params = list()
  ),
  ribbon = list(
    aes = list(fill = "black", alpha = 0.3, linewidth = NA),
    params = list()
  ),
  crossbar = list(
    aes = list(),
    params = list()
  ),
  errorbar = list(
    aes = list(),
    params = list()
  ),
  linerange = list(
    aes = list(),
    params = list()
  ),
  pointrange = list(
    aes = list(),
    params = list()
  ),
  density = list(
    aes = list(fill = "black", alpha = 0.3),
    params = list()
  ),
  dotplot = list(
    aes = list(fill = "transparent"),
    params = list()
  ),
  tile = list(
    aes = list(alpha = 1, colour = "black", linetype = "solid")
  )
)

# CONVENIENCE FUNCTIONS ---------------------------------------------------
#' Compute endpoint-aware continuous scale breaks or limits
#'
#' Internal helper used by `scale_continuous_plus()` to compute “pretty” breaks
#' while gently expanding the working range until breaks occur near both ends of
#' the original data limits.
#'
#' @param lims Numeric vector of length 2 of the incoming limits from ggplot.
#' @param n Target number of breaks passed to `scales::pretty_breaks()`.
#' @param buffer_frac Fraction of the data span used to decide whether an
#'   endpoint is close enough to a break.
#' @param Return Character string; either `"breaks"` to return computed breaks
#'   or `"limits"` to return expanded limits based on those breaks.
#'
#' @return A numeric vector of breaks or limits, as requested in `Return`.
#'
#' @keywords internal
#' @noRd
.endpoint_breaks = function(lims, n = 5, buffer_frac = 0.05, Return = c("breaks", "limits")) {

  Return = match.arg(Return)

  pretty_fn = scales::pretty_breaks(n)
  original_lo = lims[1]
  original_hi = lims[2]
  lo = original_lo
  hi = original_hi
  span = diff(lims)

  #SPAN COULD BE 0 IF LIMITS COME IN EQUAL FOR SOME REASON. THIS MAKES SURE BUFFER WOULD BE NON 0 SO WE ALWAYS GET BACK MORE THAN 1 VALUE FOR BREAKS.
  if(is.infinite(span) || span == 0) {
    eps = if(is.finite(original_lo) && original_lo != 0) {
      abs(original_lo) * 0.05
    } else { 1 }
    lo = original_lo - eps
    hi = original_hi + eps
    span = hi - lo
  }

  buffer = buffer_frac * span

  for (i in seq_len(50)) {
    brks = pretty_fn(c(lo, hi))
    got_low  = min(brks) <= (original_lo + buffer)
    got_high = max(brks) >= (original_hi - buffer)
    if (got_low && got_high) break
    if (!got_low) lo = lo - buffer
    if (!got_high) hi = hi + buffer
  }

  if(Return == "breaks") {
    return(brks)
  } else if(Return == "limits") {
    return(c(min(brks) - buffer, max(brks) + buffer))
  }
}


#' Build gridline theme adjustments from trained panel scales
#'
#' Internal helper used during the ggplotplus build stage. Inspects the trained
#' panel scales of a built plot and returns a ggplot2 theme object that keeps
#' major gridlines only for continuous position scales, using the styling stored
#' in a `GridlinesPlus` intent.
#'
#' Currently checks the first panel only. If the plot uses `coord_flip()`, the
#' x/y gridline decisions are swapped to match the rendered orientation.
#'
#' @param plot A built ggplot object.
#' @param intents A `GridlinesPlus` object containing gridline color, linewidth,
#'   and linetype settings.
#'
#' @return A ggplot2 theme object controlling major and minor panel gridlines styling instructions.
#'
#' @keywords internal
#' @noRd
.apply_gridlines_plus = function(plot, intents) {

  panel_scales = ggplot2::get_panel_scales(plot, i = 1, j = 1) #CHECK OUT THE PANEL SCALES IN THE BUILT OBJECT

  #DO THEY INHERIT A CONTINUOUS SCALE CLASS?
  x_is_cont = inherits(panel_scales$x, "ScaleContinuousPosition")
  y_is_cont = inherits(panel_scales$y, "ScaleContinuousPosition")

  #UNPACK STORED INTENTS AROUND KEEPING SOME AXES BLANK
  notx = intents@notx
  noty = intents@noty

  #NEED TO CHECK FOR AND RESPECT A COORD_FLIP BY SWAPPING THE ABOVE.
  if(inherits(plot@plot@coordinates, "CoordFlip")) {
    tmp = x_is_cont
    tmp2 = notx

    x_is_cont = y_is_cont
    notx = noty

    y_is_cont = tmp
    noty = tmp2
  }

  #CONDITIONALLY ADJUST GRIDLINES AS APPROPRIATE AND SUPPRESS ALL OTHERS.
  grid_theme = ggplot2::theme(
    panel.grid.major.x = if(x_is_cont && !notx) { #<--ENSURE INTENT OF USER IS ACKNOWLEDGED.
      ggplot2::element_line(
        colour = intents@color,
        linewidth = intents@linewidth,
        linetype = intents@linetype
      )
    } else {
      ggplot2::element_blank()
    },
    panel.grid.major.y = if(y_is_cont && !noty) {
      ggplot2::element_line(
        colour = intents@color,
        linewidth = intents@linewidth,
        linetype = intents@linetype
      )
    } else {
      ggplot2::element_blank()
    },
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank()
  )

  return(grid_theme) #RETURN THE BUILT PLOT PLUS THE NEW GRIDLINE THEME RULES.
}


#' Apply yaxis_title_plus() gtable edits
#'
#' Internal helper used during gtable construction. Removes the original y-axis
#' title grob from the left-side title slot, then inserts a new horizontal title
#' grob in a custom row near the top or bottom of the plotting area.
#'
#' This helper uses `ggplot2::get_labs()` to recover the finalized axis label
#' and `ggplot2::calc_element()` to inherit the relevant axis-title theme
#' styling. If the plot uses `coord_flip()`, the displayed vertical-axis label is
#' taken from the x scale.
#'
#' @param data A built ggplot object, usually `GGPlotPlusBuilt`.
#' @param gt A gtable produced by `ggplot2::ggplot_gtable()`.
#' @param intents A `YAxisTitlePlus` object storing title-placement intent.
#'
#' @return A modified gtable.
#'
#' @keywords internal
#' @noRd
.apply_yaxis_title_plus = function(data, gt, intents) {

  ##STOP TO CONSIDER coord_flip TO DETERMINE WHICH AXIS WE'RE RETITLING.
  real_scale = if(inherits(data$plot$coordinates, "CoordFlip")) { "x" } else { "y" }
  title_element_name = if(real_scale == "y") { "axis.title.y" } else { "axis.title.x" }

  #UNPACK USER INTENTS
  location = intents@location %||% "top"

  labs = ggplot2::get_labs(data)

  lab = if(real_scale == "y") {
    labs$y
  } else {
    labs$x
  }

  if(is.null(lab) || identical(lab, "")) {
    lab = "Placeholder. Replace w/ labs(y = ...)."
  }

  #THEN, KILL THE EXISTING TITLE GROB IN THE GTABLE SO IT DOESN'T ALSO APPEAR.
  kill_names = c("ylab-l")

  kill_idx = which(gt$layout$name %in% kill_names)
  if(length(kill_idx) > 0) { #OVERWRITE THEM WITH ZEROGROBS.
    gt$grobs[kill_idx] = replicate(length(kill_idx),
                                   ggplot2::zeroGrob(),
                                   simplify = FALSE)
  }
  #0 OUT THE WIDTHS OF THE COLS PREVIOUSLY HOLDING THOSE NAMES.
    cols2zero = unique(unlist(Map(seq.int, gt$layout$l[kill_idx], gt$layout$r[kill_idx])))
    gt$widths[cols2zero] = ggplot2::unit(0, "points")

  ###WE TRY TO LOCATE THE ROWS ABOVE WHEREVER THE AXIS-T OR BELOW THE AXIS-B ROWS ARE -- OR, ON A FACETED GRAPH, WHERE THE TOP OR BOTTOM STRIP LABELS ARE.
    axis_title_rows = which(grepl("^xlab-[tb]", gt$layout$name))
    strip_label_rows = which(grepl("^strip-[tb]", gt$layout$name))

    if(!length(axis_title_rows) & !length(strip_label_rows)) {
      return(gt) #IF SOMEHOW NONE, BREAK. ****WHY WOULD THIS OCCUR?
    }

  if(location == "top") {
    if(length(strip_label_rows) > 0 &&
       length(which(grepl("^strip-[t]", gt$layout$name)))) {

      target_axis_title_row = min(gt$layout$t[strip_label_rows]) #INSERT RIGHT WHERE THEY CURRENTLY ARE.
    } else {
      target_axis_title_row = min(gt$layout$t[axis_title_rows]) - 1 #GO ABOVE X AXIS TITLE ROW OTHERWISE.
    }
    new_title_row = target_axis_title_row + 1 #THE NEW ROW IS BELOW THE ONE I WAS TARGETING THANKS TO INCREMENTAL INDEXING.
  } else {
    if(length(strip_label_rows) > 0 & #REVERSE PATTERN ON BOTTOM.
       length(which(grepl("^strip-[b]", gt$layout$name)))) {
      target_axis_title_row = max(gt$layout$b[strip_label_rows])
    } else {
      target_axis_title_row = max(gt$layout$b[axis_title_rows]) + 1
    }
    new_title_row = target_axis_title_row - 1
  }

    #FIND THE COLUMN IN WHICH THE AXIS-L CONTENT WAS ORIGINALLY--THIS IS WHAT WE WILL JUSTIFY THE CONTENTS OF THE NEW TITLE TO ON THE LEFT-HAND SIDE.
  axis_l_cols = which(grepl("^axis-l", gt$layout$name))
  panel_cols = which(grepl("^panel", gt$layout$name))

  title_col = if(length(axis_l_cols) > 0) {
    min(gt$layout$l[axis_l_cols])  #GET THE MINIMUM ONE, IF THERE ARE MANY.
  } else {
    min(gt$layout$l[panel_cols]) #OTHERWISE, DEFAULT TO THE COL TO THE FAR LEFT OF THE PANEL...****IS THIS NECESSARY?
  }

  #THEN, INSERT ENTIRELY NEW ROW FOR THE TITLE, EITHER ON TOP OR ON BOTTOM.
  gt = gtable::gtable_add_rows(gt,
                               heights = ggplot2::unit(16, "pt"),
                               pos = target_axis_title_row)

  #THEN, BEGIN BUILDING THE TEXT GROB. SHOULD USE THE THEME STYLES FROM THE PREVIOUS TITLE.
  el = ggplot2::calc_element(title_element_name, data$plot$theme)
  gp = .ggplus_element_to_gpar(el) #JUST TRANSLATES THEME ARG NAMES TO GPAR ARG NAMES.

  vjust_val = 0.5

  #ACTUALLY ADD THE GROB
  gt = gtable::gtable_add_grob(
    gt,
    grob = grid::textGrob(
      lab,
      x = 0, y = 0.5,
      hjust = 0, vjust = vjust_val,
      rot = 0,
      gp = gp
    ),
    t = new_title_row,
    b = new_title_row,
    l = title_col,
    name = paste0("ggplotplus-", real_scale, "-title"),
    clip = "off"
  )

  return(gt)

}

#' Convert a ggplot2 theme text element to grid graphical parameters
#'
#' Internal helper for translating selected text-element settings from ggplot2's
#' theme system into a `grid::gpar()` object suitable for manually constructed
#' grobs.
#'
#' @param el A text theme element, usually returned by `ggplot2::calc_element()`.
#'
#' @return A `grid::gpar()` object containing color, font size, font face, font
#'   family, and line-height settings.
#'
#' @keywords internal
#' @noRd
.ggplus_element_to_gpar = function(el) {
  grid::gpar(
    col        = el$colour %||% el$color %||% "black",
    fontsize   = el$size %||% 11,
    fontface   = el$face %||% "plain",
    fontfamily = el$family %||% "",
    lineheight = el$lineheight %||% 0.9
  )
}


#' Extract a partially matched argument from a named list
#'
#' Internal helper that searches a list of named arguments for a single
#' partially matching name using `pmatch()`. Returns the matched value if found.
#'
#' @param args A named list of arguments.
#' @param target A character string giving the target argument name.
#'
#' @return The matched argument value, or `NULL` if no match is found.
#'
#' @keywords internal
#' @noRd
.partial_match_user_arg = function(args, target) {
  nms = names(args) #FIND ALL NAMED ARGUMENTS.

  if (is.null(nms)) { #IF NONE STOP--NO MATCHING POSSIBLE.
    return(NULL)
  }

  #INDEXES OF NAMED, PARTIALLY MATCHING ARGUMENTS.
  idx = which(!is.na(nms) & pmatch(nms, target, nomatch = 0L) > 0)

  if (length(idx) > 1) { #TOO MANY? STOP.
    stop(sprintf("Multiple arguments match '%s'.", target), call. = FALSE)
  }

  #IF EXACTLY 1, FIND THAT MATCHING ARGUMENT OR GIVE UP.
  if (length(idx) == 1) args[[idx]] else NULL
}


#' Remove a partially matched argument from a named list
#'
#' Internal helper that removes a single partially matching argument from a
#' named list using `pmatch()`. Intended for sanitizing user inputs before
#' forwarding arguments to ggplot2 functions.
#'
#' @param args A named list of arguments.
#' @param target A character string giving the target argument name.
#'
#' @return The input list with the matching argument removed, if present.
#'
#' @keywords internal
#' @noRd
.remove_partial_match_user_arg = function(args, target) {

  nms = names(args)

  # If no names, nothing to do
  if (is.null(nms)) {
    return(args)
  }

  # Identify matches (same logic as your fixed helper)
  idx = which(!is.na(nms) & pmatch(nms, target, nomatch = 0L) > 0)

  # If multiple matches, mirror your existing behavior
  if (length(idx) > 1) {
    stop(sprintf("Multiple arguments match '%s'.", target), call. = FALSE)
  }

  # If exactly one match, drop it
  if (length(idx) == 1) {
    args = args[-idx]
  }

  return(args)
}


#' Normalize viridis palette specification
#'
#' Internal helper that converts single-letter viridis palette codes (A–H)
#' to their full names.
#'
#' @param x A character vector giving a viridis palette name or code.
#'
#' @return A character string giving the normalized viridis palette name.
#'
#' @keywords internal
#' @noRd
.normalize_viridis = function(x) {
  map = c(A="magma", B="inferno", C="plasma", D="viridis",
          E="cividis", F="rocket", G="mako", H="turbo")
  if (length(x) == 1 && x %in% names(map)) map[[x]] else x
}


#' Create a discrete viridis palette function
#'
#' Internal helper that returns a function generating discrete color palettes
#' using `viridisLite::viridis()`, suitable for ggplot2 theme palette settings.
#'
#' @param option Viridis palette name.
#' @param begin,end Length one numeric values between 0 and 1 controlling palette endpoints.
#'
#' @return A function accepting `n` and returning a vector of colors.
#'
#' @keywords internal
#' @noRd
.make_discrete_palette = function(option, begin, end) {
  function(n) {
    viridisLite::viridis(
      n = n,
      option = option,
      begin = begin,
      end = end
    )
  }
}

#' Create a continuous viridis palette function
#'
#' Internal helper that returns a continuous palette function using
#' `scales::pal_viridis()`, suitable for ggplot2 theme palette settings.
#'
#' @param option Viridis palette name.
#' @param begin,end Length one numeric values between 0 and 1 controlling palette endpoints.
#'
#' @return A palette function mapping numeric values between 0 and 1 to colors.
#'
#' @keywords internal
#' @noRd
.make_continuous_palette = function(option, begin, end) {
  scales::pal_viridis(
    option = option,
    begin = begin,
    end = end
  )
}


#' Construct ggplotplus palette theme settings
#'
#' Internal helper that builds a `ggplot2::theme()` call configuring discrete
#' and continuous color/fill palettes using viridis-based palette functions.
#'
#' @param palette_discrete,palette_continuous Viridis palette names or codes.
#' @param begin_discrete,end_discrete Numeric endpoints for a discrete palette.
#' @param begin_continuous,end_continuous Numeric endpoints for a continuous palette.
#'
#' @return A ggplot2 theme object specifying palette settings.
#'
#' @keywords internal
#' @noRd
.theme_plus_palettes = function(palette_discrete,
                                palette_continuous,
                                begin_discrete,
                                end_discrete,
                                begin_continuous,
                                end_continuous) {

  palette_discrete = .normalize_viridis(palette_discrete)
  palette_continuous = .normalize_viridis(palette_continuous)

  disc = .make_discrete_palette(palette_discrete, begin_discrete, end_discrete)
  cont = .make_continuous_palette(palette_continuous, begin_continuous, end_continuous)

  return(ggplot2::theme(
    palette.colour.discrete = disc,
    palette.fill.discrete = disc,
    palette.colour.continuous = cont,
    palette.fill.continuous = cont
  ))
}

#' Legend-Position Theme Conditional Logic
#'
#' Builds a small theme fragment for customizing `"top"` vs `"right"` legends.
#'
#' @inheritParams theme_plus
#' @return A ggplot2 theme object.
#' @keywords internal
#' @noRd
.determine_legend_theme = function(legend_pos = "top") {

  if(legend_pos == "top") {
    ggplot2::theme(
      legend.key.width = ggplot2::unit(1.5, "cm"),
      legend.key.height = ggplot2::unit(0.8, "cm"),
      legend.title = ggplot2::element_text(margin = ggplot2::margin(l = 15)),
      legend.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5),
      plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5),
      legend.box.just = "right",
      legend.justification = "right",
      legend.key.justification = "right",
      legend.title.position = "right",
      legend.key.spacing.x = ggplot2::unit(0.5, "cm"),
      legend.position = "top",
      legend.direction = "horizontal"
    )
  } else if(legend_pos == "right") {

    ggplot2::theme(
      legend.key.height = ggplot2::unit(1.5, "cm"),
      legend.key.width = ggplot2::unit(0.8, "cm"),
      legend.title = ggplot2::element_text(margin = ggplot2::margin(b = 15), hjust = 0.5),
      legend.box.just = "right",
      legend.margin = ggplot2::margin(t = 5, r = 10, b = 5, l = 5),
      plot.margin = ggplot2::margin(t = 5, r = 10, b = 5, l = 5),
      legend.justification = "right",
      legend.key.spacing.y = ggplot2::unit(0.5, "cm"),
      legend.position = "right",
      legend.direction = "vertical")
  }
}


#' Check whether a geom parameter is already set
#'
#' Internal helper used when applying `theme_plus()` geom defaults. Determines
#' whether a layer already has a geom parameter set so ggplotplus does not
#' overwrite explicit user intent.
#'
#' Includes a special case for violin `quantile_gp`, which exists by default in
#' an inactive state and should only count as set when quantile styling has
#' actually been supplied by the user.
#'
#' @param layer A ggplot2 layer object.
#' @param param Character string naming the geom parameter to check.
#'
#' @return Logical; `TRUE` if the parameter should be treated as already set.
#'
#' @keywords internal
#' @noRd
.param_is_already_set = function(layer, param) {

  if (param == "quantile_gp") {
    qgp = layer$geom_params$quantile_gp

    hasbeenset = !is.null(qgp) &&
      (
        (!is.null(qgp$linetype) && !is.na(qgp$linetype) && qgp$linetype != 0) ||
          !is.null(qgp$colour) ||
          !is.null(qgp$linewidth)
      )

    return(hasbeenset)
  }

  param %in% names(layer$geom_params)
}



#' Check whether an aesthetic is mapped locally
#'
#' Internal helper used by `geom_point_plus()` to detect whether a specific
#' aesthetic is mapped in a local `mapping` argument or in an unnamed `aes()`
#' object passed through `...`.
#'
#' This does not currently inspect global plot mappings.
#'
#' @param mapping A local ggplot2 aesthetic mapping, usually from `aes()`.
#' @param dots A list of additional arguments passed through `...`.
#' @param aes_name Character string naming the aesthetic to detect.
#'
#' @return Logical; `TRUE` if the aesthetic is mapped locally.
#'
#' @keywords internal
#' @noRd
.has_mapped_aes = function(mapping, dots, aes_name) {

  # collect all candidate aes objects
  aes_objs = list()

  # 1) explicit mapping arg
  if (!is.null(mapping) && inherits(mapping, "uneval")) {
    aes_objs = c(aes_objs, list(mapping))
  }

  # 2) unnamed aes(...) in ...
  for (obj in dots) {
    if (inherits(obj, "uneval")) {
      aes_objs = c(aes_objs, list(obj))
    }
  }

  if (length(aes_objs) == 0) return(FALSE)

  # check if any aes contains the target
  any(vapply(aes_objs, function(a) aes_name %in% names(a), logical(1)))
}


#' Nudge a top-positioned legend downward
#'
#' Internal helper that returns a `ggplot2::theme()` adjustment which shifts a
#' legend (box) positioned at the top of a plot slightly downward by applying a
#' negative bottom margin to the legend box.
#'
#' This is used by `yaxis_title_plus()` to prevent wasted space between the relocated
#' y-axis title and a top-positioned legend (box).
#'
#' @param howMuch Numeric value (in points) controlling how far the legend is
#'   nudged downward. Larger values move the legend further.
#'
#' @return A ggplot2 theme object adjusting `legend.box.margin`.
#'
#' @keywords internal
#' @noRd
.nudge_top_legend_down = function(howMuch = 20) {

  ggplot2::theme(legend.box.margin = ggplot2::margin(b = -howMuch, r = 5, t = 5, l = 5))

}


#' Standardize ggplotplus shape names
#'
#' Internal helper to normalize shape identifiers used by
#' \code{geom_point_plus()}. Converts numeric shape codes
#' corresponding to base R's fillable point shapes (21--25)
#' into their ggplotplus string equivalents.
#'
#' This ensures consistent downstream handling regardless of
#' whether users supply shapes as numbers (e.g., \code{21}) or
#' names (e.g., \code{"circle"}).
#'
#' @param shape A vector of shape identifiers. May be numeric,
#'   character, or factor.
#'
#' @return A character vector of standardized shape names.
#'
#' @keywords internal
.standardize_pointplus_shape_names = function(shape) {

  if(is.null(shape)) { #DEFENDS--IF NULL, THE DEFAULT, JUST RETURN NULL, AS THAT IS WHAT IS EXPECTED DOWNSTREAM.
    return(NULL)
  }

  validshapeslookup = c(
    "21" = "circle",
    "22" = "square",
    "23" = "diamond",
    "24" = "triangle_up",
    "25" = "triangle_down"
  )

  shape = as.character(shape)

  use_lookup = shape %in% names(validshapeslookup)

  shape[use_lookup] = validshapeslookup[shape[use_lookup]]

  shape
}


#' Validate a geom_point_plus() shape definition
#'
#' Internal helper used by \code{add_shape_plus()} to check that a custom
#' point shape has the structure needed by the ggplotplus shape-drawing
#' machinery.
#'
#' @param shape A proposed shape definition. Must be a data frame with columns
#'   \code{x}, \code{y}, and \code{piece}.
#' @param name Shape name, used by callers.
#'
#' @return A cleaned shape data frame containing only \code{x}, \code{y}, and
#'   \code{piece}. The \code{piece} column is converted to sequential integers if
#'   it wasn't already formatted thusly.
#'
#' @keywords internal
.validate_pointplus_shape = function(shape, name = NULL) {

  #VARIOUS SAFETY CHECKS TO ENSURE WE GOT THE INPUTS WE NEEDED W/ HELPFUL ERROR MESSAGES IF NOT.
  required_cols = c("x", "y", "piece")

  if(!is.data.frame(shape)) {
    stop("`shape` must be a data frame containing three columns named 'x', 'y', and 'piece'. See ?add_shape_plus for details.", call. = FALSE)
  }

  missing_cols = setdiff(required_cols, names(shape))

  if(length(missing_cols) > 0) {
    stop(
      "`shape` is missing the following required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if(!is.numeric(shape$x) || !is.numeric(shape$y)) {
    stop("`shape$x` and `shape$y` must both be numeric.", call. = FALSE)
  }

  if(any(!is.finite(shape$x)) || any(!is.finite(shape$y))) {
    stop("`shape$x` and `shape$y` must contain only finite numeric values.", call. = FALSE)
  }

  if(any(is.na(shape$piece))) {
    stop("`shape$piece` cannot contain missing values. These should be integers starting at 1.", call. = FALSE)
  }

  pieces = split(shape, shape$piece) #CHOPS INTO A LIST BY PIECE VAL.

  n_vertices = vapply(pieces, nrow, integer(1)) #APPLIES NROW TO EACH.

  if(any(n_vertices < 3)) {
    stop("Each `piece` of your shape must contain at least 3 points.", call. = FALSE)
  }

  #****THIS VALIDATES THE FIRST REQUIREMENT BUT NOT THE SECOND...THAT'S PROBABLY OK? MORE LIKELY YOU WILL FAIL BOTH IF YOU FAIL THE FIRST. CAN FINE-TUNE LATER.
  if(max(abs(c(shape$x, shape$y))) > 1) {
    warning(
      "`shape` coordinates extend beyond +/-1. ",
      "Point shapes are expected to be centered on 0 and scaled to roughly +/-0.4.",
      call. = FALSE
    )
  }

  shape = shape[, required_cols, drop = FALSE] #GETS RID OF ANY EXTRA COLS.

  shape$piece = as.integer(as.factor(shape$piece)) #RENUMBERS THESE TO BE INTEGERS STARTING AT 1 IF THEY AREN'T ALREADY.

  return(shape)
}


#' Get registered geom_point_plus() shapes
#'
#' Internal helper that returns the current ggplotplus point-shape registry,
#' including both built-in package shapes and any shapes added during the
#' current R session with \code{add_shape_plus()}.
#'
#' @return A named list of point-shape data frames.
#'
#' @keywords internal
.pointplus_shapes = function() {

  .initialize_pointplus_shape_registry()

  shape_names = .pointplus_shape_registry$.order

  shape_names = shape_names[
    vapply(shape_names, function(x) {
      exists(x, envir = .pointplus_shape_registry, inherits = FALSE)
    }, logical(1))
  ]

  stats::setNames(
    lapply(shape_names, function(x) {
      get(x, envir = .pointplus_shape_registry, inherits = FALSE)
    }),
    shape_names
  )
}


#' Safely check whether an S7 property is TRUE
#'
#' Internal helper that safely checks whether an object:
#' \enumerate{
#'   \item exists and is not \code{NULL},
#'   \item is an S7 object,
#'   \item contains a specified property, and
#'   \item has that property set to \code{TRUE}.
#' }
#'
#' This helper is primarily used to avoid errors when optional
#' ggplotplus S7 components may not (yet) exist on a plot object.
#'
#' @param object An object that may or may not be an S7 object.
#' @param prop A single character string giving the property name
#'   to check.
#'
#' @return A logical scalar. Returns \code{TRUE} only if the object
#'   is a valid S7 object, the property exists, and the property
#'   value is exactly \code{TRUE}. Otherwise returns \code{FALSE}.
#'
#' @keywords internal
.s7_prop_is_true = function(object, prop) {

  if(is.null(object)) {
    return(FALSE)
  }

  if(!S7::S7_inherits(object, S7::S7_object)) {
    return(FALSE)
  }

  if(!S7::prop_exists(object, prop)) {
    return(FALSE)
  }

  isTRUE(S7::prop(object, prop))
}
