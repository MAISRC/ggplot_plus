# IMPORT COMMANDS ---------------------------------------------------------

#' @importFrom rlang %||% list2
#' @import ggplot2
#' @importFrom ggplot2 update_ggplot class_ggplot ggplot_build ggplot_gtable
#' @importFrom polyclip polyclip
#' @importFrom scales pretty_breaks pal_viridis
#' @importFrom viridisLite viridis
NULL


# CUSTOM OBJECTS ----------------------------------------------------------

#' A new palette of distinct shapes made especially for `geom_point_plus`
#'
#' Call this object to generate a `ggplot` showing the names and features of each of the shapes available to `ggplotplus::geom_point_plus`.
#'
#' @return A named list.
#' @export
geom_point_plus_shapes = function(){
  ggplot2::ggplot(data = data.frame(x = rep(c(0.5,1.5,2.5), each = 3),
                                    y = rep(c(1,2,3), times = 3),
                                    shape = factor(1:9))) +
    geom_point_plus(ggplot2::aes(x = x, y = y, shape = shape, fill = shape), chosen_shapes = c("squircle", "octagon", "flower", "economy", "cross", "waffle", "oval", "sunburst", "lotus"),
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
    ggplot2::annotate("text", x = 1.5, y = 1.75, label = "cross", size = 5, fontface = 'bold') +
    ggplot2::annotate("text", x = 1.5, y = 2.75, label = "waffle", size = 5, fontface = 'bold') +
    ggplot2::annotate("text", x = 2.5, y = 0.75, label = "oval", size = 5, fontface = 'bold') +
    ggplot2::annotate("text", x = 2.5, y = 1.75, label = "sunburst", size = 5, fontface = 'bold') +
    ggplot2::annotate("text", x = 2.5, y = 2.75, label = "lotus", size = 5, fontface = 'bold')
}

# Defining custom shapes via series of coordinates ------------------------

#EVERY SHAPE ADDED IS A SERIES OF X/Y COORDINATES TO BE CONNECTED VIA A PATH. EVERYTHING  IS CENTERED AROUND 0,0 AND SHOULD BE VALUES BETWEEN ~ -1 AND 1, WITH A ROUGH DIAMETER OF 0.4. PIECE CORRESPONDS TO WHICH ELEMENTS WILL BE CONSIDERED A "HOLE" INSIDE OF OTHER ELEMENTS, WITH ALL ELEMENTS HAVING A PIECE VALUE OTHER THAN 1 BECOMING HOLES.

#SHAPE 1: WAFFLE. THIS SHAPE IS CLOSED, INTERSECTIONAL, AND SPIKY.
{
  waffle = data.frame(
    x = c(-5, -5, 5, 5, -3, -1, -1, -3, -3, -1, -1, -3, 1, 3, 3, 1, 1, 3, 3, 1) / 12,
    y = c(-5, 5, 5, -5, -3, -3, -1, -1, 1, 1, 3, 3, -3, -3, -1, -1, 1, 1, 3, 3) / 12,
    piece = rep(1:5, each = 4)
  )
}

#SHAPE 2: CROSS. THIS SHAPE IS INTERMEDIATE IN OPENNESS, INTERSECTIONALITY, AND SPIKINESS.
{
  arm_len = 0.40
  arm_thick = 0.12

  l = arm_len
  t = arm_thick

  xy = matrix(c(
    -l, -t,
    -t, -t,
    -t, -l,
    t, -l,
    t, -t,
    l, -t,
    l,  t,
    t,  t,
    t,  l,
    -t,  l,
    -t,  t,
    -l,  t
  ), byrow = TRUE, ncol = 2)

  cross = data.frame(x = xy[,1],
                     y = xy[,2],
                     piece = 1)
}

#SHAPE 3: SUNBURST. THIS SHAPE IS OPEN, SPIKY, AND INTERSECTIONAL.
{
  n = 9; r = 0.4; base_width = 0.12

  angles = seq(0, 2 * pi, length.out = n + 1)[-1]  # angles for each triangle
  half_base_angle = asin(base_width / (2 * r))     # half-angle that spans base

  triangles = lapply(seq_along(angles), function(i) {
    center = c(0, 0)
    angle = angles[i]

    base_left = c(r * cos(angle - half_base_angle),
                  r * sin(angle - half_base_angle))
    base_right = c(r * cos(angle + half_base_angle),
                   r * sin(angle + half_base_angle))

    data.frame(
      x = c(base_left[1], center[1], base_right[1]),
      y = c(base_left[2], center[2], base_right[2]),
      piece = i
    )
  })

  sunburst = do.call(rbind, triangles)
}

#SHAPE 4: LOTUS (ALSO CALLED THE "EGG OF LIFE"). THIS SHAPE IS OPEN, ROUNDED, AND INTERSECTIONAL.
{
  outer_r  = 0.40        # radius of big disk
  inner_r  = 0.09        # radius of each hole
  r_offset = 0.24        # distance from origin to the six outer-hole centers
  n_pts    = 100
  theta    = seq(0, 2*pi, length.out = n_pts)

  outer_circle = data.frame(
    x = outer_r * cos(theta),
    y = outer_r * sin(theta),
    piece = 1
  )

  center_positions = rbind(
    c(0, 0),
    t(sapply(0:5, function(i) {
      angle = i * pi/3
      c(cos(angle), sin(angle)) * r_offset
    }))
  )

  #Build the seven holes
  hole_circles = do.call(rbind, lapply(seq_len(nrow(center_positions)), function(i) {
    cx = center_positions[i, 1]
    cy = center_positions[i, 2]
    data.frame(
      x     = cx + inner_r * cos(theta),
      y     = cy + inner_r * sin(theta),
      piece = i + 1                                # pieces 2–8 to make these holes.
    )
  }))

  egg_of_life = rbind(outer_circle, hole_circles)
}

#SHAPE 5: ECONOMY (SO-NAMED FOR THE SQUARE-IN-A-SQUARE OR "ECONOMY" QUILT BLOCK). THIS SHAPE IS OPEN, SPIKY, AND NON-INTERSECTIONAL.
{
  r = 0.39
  outer = data.frame(
    x = c(-r, r, r, -r),
    y = c(-r, -r, r, r),
    piece = 1
  )

  # Inner diamond (rotated square)
  diamond = data.frame(
    x = c(0, r, 0, -r),
    y = c(r, 0, -r, 0),
    piece = 2
  )

  economy = rbind(outer, diamond)
}

#SHAPE 6: HOLLOW OVAL. THIS SHAPE IS OPEN, ROUNDED, AND NON-INTERSECTIONAL.
{
  rx  = 0.45
  ry  = 0.3
  th  = 0.15
  n   = 100
  ang = pi/4          # 45 degrees in radians turns the element diagonal.

  theta = seq(0, 2*pi, length.out = n)

  rotate = function(dx, dy, a = ang) {
    cbind(
      x =  dx*cos(a) - dy*sin(a),
      y =  dx*sin(a) + dy*cos(a)
    )
  }

  # outer ring
  outer_xy = rotate(rx * cos(theta),   ry * sin(theta))
  outer = data.frame(outer_xy, piece = 1)

  # inner ring (reverse winding so the hole stays punched out)
  inner_xy = rotate((rx - th) * cos(rev(theta)),
                    (ry - th) * sin(rev(theta)))
  inner = data.frame(inner_xy, piece = 2)

  oval = rbind(outer, inner)
}


#SHAPE 7: SQUIRCLE (ROUNDED SQUARE). THIS SHAPE IS CLOSED, ROUNDED, AND NON-INTERSECTIONAL.
{
  a = 0.4; b = 0.4; n = 4; pts = 200
  t  = seq(0, 2*pi, length.out = pts)

  x = a * sign(cos(t)) * abs(cos(t))^(2/n)
  y = b * sign(sin(t)) * abs(sin(t))^(2/n)

  ang = pi/4                         # 45°
  xR  =  x*cos(ang) - y*sin(ang)
  yR  =  x*sin(ang) + y*cos(ang)

  squircle = data.frame(x = xR, y = yR, piece = 1)
}


#SHAPE 8: FLOWER, I.E. SEVEN FILLED AND PARTIALLY OVERLAPPING CIRCLES. THIS SHAPE IS CLOSED, ROUNDED, AND INTERSECTIONAL.
{
  r_hub  = 0.32      # center circle radius
  r_tip  = 0.15      # rounded arm-tip radius
  offset = 0.32     # distance from origin to each tip-circle center
  nSeg   = 32

  # helper: circle as list(x, y) with *n_seg* distinct vertices
  circle = function(cx, cy, r) {
    t = seq(0, 2 * pi, length.out = nSeg + 1)[-1L]   # drop 0-rad duplicate
    list(x = cx + r * cos(t),
         y = cy + r * sin(t))
  }

  hub = circle(0, 0, r_hub)

  angles  = seq(0, 2 * pi - pi / 3, length.out = 6)
  petals  = lapply(angles, function(a) { circle(offset * cos(a),
                                                 offset * sin(a),
                                                 r_tip)})

  union_fun = function(a, b)
    polyclip::polyclip(a, b,
                       op        = "union",
                       operation = "union")

  outline = Reduce(union_fun, petals, init = hub)[[1L]]

  ## ----- close the ring ----------------------------------------------------
  if (outline$x[1L] != outline$x[length(outline$x)] ||
      outline$y[1L] != outline$y[length(outline$y)]) {
    outline$x = c(outline$x, outline$x[1L])
    outline$y = c(outline$y, outline$y[1L])
  }

  flower = data.frame(x = outline$x,
                      y = outline$y,
                      piece = 1L)
}


#SHAPE 9: OCTAGON. THIS SHAPE IS CLOSED, SPIKY, AND NON-INTERSECTIONAL.
{
  r = 0.45     # “radius” (distance from center to each vertex)
  n = 8      # number of sides --could be adjusted
  rotate = pi/n   # optional rotation (0 puts a point at 0°; pi/n flattens the top)

  theta = seq(0, 2*pi - 2*pi/n, length.out = n) + rotate

  octagon = data.frame(
    x     = r * cos(theta),
    y     = r * sin(theta),
    piece = 1
  )
}

#' A new palette of shapes to use in `ggplot2` scatterplots.
#'
#' This named list contains elements with x and y coordinates that, when corrected, will draw new shapes to be used as points in `ggplot2` scatterplots. The `piece` attribute of each element dictates whether a region of the resulting shape will be solid or a "hole" in the final shape.
#'
#' @return A named list.
#' @export
ggplotplus_shapes_list = list(
  squircle = squircle,
  lotus = egg_of_life,
  flower = flower,
  oval = oval,
  octagon = octagon,
  sunburst = sunburst,
  waffle = waffle,
  economy = economy,
  cross = cross
)


#' Default settings for geometry layers created by `geom_plus()`
#'
#' A named list of default aesthetics used by `geom_plus()` to control styling of the resulting geometry layers.
#'
#' Most geom-specific default aesthetics are set in `theme_plus()`; what is set here is what can't be set there.
#'
#' @format A named list with elements like "point", "jitter", "boxplot", etc., corresponding to commonly used ggplot2 geometries. Use names(geom_plus_defaults) for a full list.
#' @keywords internal
#' @noRd
.geom_plus_defaults = function(){
  list(
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
}


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

  #NEED TO CHECK FOR AND RESPECT A COORD_FLIP BY SWAPPING THE ABOVE.
  if(inherits(plot@plot@coordinates, "CoordFlip")) {
    tmp = x_is_cont
    x_is_cont = y_is_cont
    y_is_cont = tmp
  }

  #CONDITIONALLY ADJUST GRIDLINES AS APPROPRIATE AND SUPPRESS ALL OTHERS.
  grid_theme = ggplot2::theme(
    panel.grid.major.x = if (x_is_cont) {
      ggplot2::element_line(
        colour = intents@color,
        linewidth = intents@linewidth,
        linetype = intents@linetype
      )
    } else {
      ggplot2::element_blank()
    },
    panel.grid.major.y = if (y_is_cont) {
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

  ###WE TRY TO LOCATE THE ROWS ABOVE WHEREVER THE AXIS-T OR BELOW THE AXIS-B ROWS ARE.
    axis_title_rows = which(grepl("^xlab-t", gt$layout$name))
    if(!length(axis_title_rows)) {
      return(gt) #IF SOMEHOW NONE, BREAK. ****WHY WOULD THIS OCCUR?
    }

  target_axis_title_row = if(location == "top") {
    min(gt$layout$t[axis_title_rows]) - 1 #GO ABOVE AT TOP, BELOW AT BOTTOM.
  } else {
    max(gt$layout$b[axis_title_rows]) + 1
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
    t = target_axis_title_row,
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
