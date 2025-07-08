# Backend functions -------------------------------------------------------

#' An backend version of `geom_point()` that can access distinctive shapes.
#'
#' This function behaves much like `ggplot2`'s `geom_point()` function except that it allows access to a palette of nine new shapes that vary in their openness, spikiness, and intersectionality, making them more easily distinguished. This function is not meant to be called directly--instead, `geom_point_plus()` calls and modifies this function and is the intended function for users.
#'
#' @param mapping Set of aesthetic mappings created by aes(), as in `ggplot2::geom_point()`.
#' @param data The data to be displayed in this layer, as in `ggplot2::geom_point()`.
#' @param stat The statistical transformation to use on the data for this layer, as in `ggplot2::geom_point()`.
#' @param position A position adjustment to use on the data for this layer, as in `ggplot2::geom_point()`.
#' @param shapes A named list of custom shapes to be drawn in place of `ggplot2`'s standard palette of shapes.
#' @param ... Other arguments passed on to layer()'s params argument, as in `ggplot2::geom_point()`.
#' @param na.rm Logical value controlling whether missing values should be removed from the data with a warning or silently, as in `ggplot2::geom_point()`.
#' @param show.legend Logical value controlling whether this layer should be included in the legends, as in `ggplot2::geom_point()`.
#' @param inherit.aes Logical for whether the default aesthetics should be overridden rather than combined with the provided aesthetics, as in `ggplot2::geom_point()`.
#' @return A ggplot2 layer object.
#' @export
geom_point2 = function(mapping = NULL,
                       data = NULL,
                       stat = "identity",
                       position = "identity",
                       shapes = shapes.list, #THE KEY INPUT--THIS IS THE NEW PALETTE OF SHAPES AVAILABLE.
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE){

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointPlus, #IT CALLS THE NEW GEOMPOINTPLUS PROTO.
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(na.rm = na.rm, shapes = shapes, ...)
  )

}



#' An alternative version of `ggplot2`'s geomPoint proto that incorporates new, distinctive shapes.
#'
#' This ggplot proto object is called internally by `geom_point2()` and inherits most, but not all, of its methods from those used in `ggplot2`'s standard `geomPoint` proto. However, it has different default aesthetics, a different shapes palette, and can draw these new shapes in a legend. This function is not meant to be called by the user; for that, `geom_point_plus()` is intended.
#'
#' @return A `ggplot2` ggproto class object.
#' @export
GeomPointPlus = ggplot2::ggproto(
  "point_plus",
  ggplot2::GeomPoint, #THIS NEW GEOM PROTO INHERITS PROPERTIES AND METHODS FROM GEOMPOINT, BUT WILL OVERRIDE THREE OF ITS MAIN ATTRIBUTES: DRAW_PANEL, DRAW_KEY, AND DEFAULT AES. IT BEHAVES EXACTLY LIKE GEOM_POINT UNLESS A CHARACTER-BASED SHAPE INPUT IS PROVIDED THAT MATCHES ONE IN THE CUSTOM SHAPES LIST.
  #DRAW PANEL IS THE METHOD THAT DICTATES WHAT EXACTLY GETS DRAWN IN THE PLOT PANEL. WE OVERRIDE IT SO THAT OUR CUSTOM SHAPES COME THROUGH.
  draw_panel = function(self,
                        data,
                        panel_params,
                        coord,
                        shapes,
                        na.rm = FALSE) {

    #THIS TRANSLATES A CHARACTER SHAPE NAME, LIKE "STAR", INTO A NUMERIC CODE TO BE USED IN A LOOKUP OPERATION USING OUR SHAPES LIST.
    if (is.character(data$shape)) {
      data$shape = names(shapes)[match(data$shape, names(shapes))]
    }

    coords = coord$transform(data, panel_params)
    stroke_size = coords$stroke
    stroke_size[is.na(stroke_size)] = 0

    #IF THE SHAPE INPUT WAS NUMERIC, IT MEANS IT'S PROBABLY A CONVENTIONAL GGPLOT SHAPE CODE, IN WHICH CASE, WE JUST DO WHAT GEOM_POINT ALREADY DOES, WHICH IS DRAW POINTS USING THOSE CONVENTIONAL SHAPES, ENSURING THAT GEOM_POINT'S BASIC FUNCTIONALITY IS RETAINED (THOUGH IT'S NOT OUR INTENTION THAT USERS USE GEOM_POINT_PLUS WHEN CUSTOM SHAPES ARE NOT NEEDED).
    if (!is.character(data$shape)) {
      return(grid::pointsGrob(
        coords$x,
        coords$y,
        pch = coords$shape,
        gp = grid::gpar(
          col = ggplot2::alpha(coords$colour, coords$alpha),
          fill = ggplot2::fill_alpha(coords$fill, coords$alpha),
          fontsize = coords$size * .pt + stroke_size * .stroke /
            2,
          lwd = coords$stroke * .stroke /
            2
        )
      ))
    }
    #OTHERWISE, THE METHOD ASSUMES YOU WANT TO USE THE CUSTOM SHAPES, WHICH IT'LL DRAW BY CONNECTING A SERIES OF COORDINATES USING A PATHGROB.
    #EVERY SHAPE INPUT NEEDS TO BE IN THE SHAPES LIST, AND IT NEEDS TO BE A NAMED LIST WITH $X $Y AND $PIECE ELEMENTS. THE X AND Y VALUES SHOULD BE CENTERED AROUND 0,0 AND BE SCALED TO ~0.4 RADIUS TO BE OF SIMILAR SIZES TO EACH OTHER.
    g = Map(
      function(x,
               y,
               shape,
               size,
               color,
               fill,
               strokewidth) {
        dat = shapes[[shape]]
        xvals = grid::unit(x, "npc") + grid::unit(dat$x * size * 2, "points")
        yvals = grid::unit(y, "npc") + grid::unit(dat$y * size * 2, "points")
        grid::pathGrob(xvals,
                       yvals,
                       id = dat$piece,
                       rule = "evenodd",
                       gp = grid::gpar(
                         col = color,
                         fill = fill,
                         lwd = strokewidth
                       ))
      },
      coords$x,
      coords$y,
      coords$shape,
      coords$size * ggplot2::.pt + stroke_size * ggplot2::.stroke /
        2,
      ggplot2::alpha(coords$colour, coords$alpha),
      ggplot2::fill_alpha(coords$fill, coords$alpha),
      coords$stroke * .stroke / 2
    )
    do.call(grid::grobTree, c(g, list(name = "geom_point2")))
  },

  #DRAW_KEY IS THE METHOD USED TO DRAW THE SYMBOLS IN THE LEGENDS IT FOLLOWS THE SAME BEHAVIOR AS DRAW_PANEL FOR THE MOST PART EXCEPT IT ONLY DRAWS A SINGLE SYMBOL PER SHAPE NEEDED AT COORDINATES OF 0.5, 0.5 IN NPC UNITS.
  draw_key = function (data, params, size) {
    if (is.null(data$shape)) {
      data$shape = 21
    }

    stroke_size = data$stroke %||% 0.5
    stroke_size[is.na(stroke_size)] = 0

    size = data$size %||% 5

    if (!is.character(data$shape)) {
      return(grid::pointsGrob(
        0.5,
        0.5,
        pch = data$shape,
        gp = grid::gpar(
          col = ggplot2::alpha(data$colour, data$alpha),
          fill = ggplot2::fill_alpha(data$fill, data$alpha),
          fontsize = size * ggplot2::.pt + stroke_size * ggplot2::.stroke / 2,
          lwd = stroke_size * ggplot2::.stroke / 2
        )
      ))
    }

    g = Map(
      function(shape, size_val, color, fill, strokewidth) {
        dat = params$shapes[[shape]]
        xvals = grid::unit(0.5, "npc") + grid::unit(dat$x * size_val, "points")
        yvals = grid::unit(0.5, "npc") + grid::unit(dat$y * size_val, "points")
        grid::pathGrob(
          xvals, yvals, dat$piece,
          rule = "evenodd", #THIS PART IS KEY--IT ALLOWS US TO PUNCH HOLES IN SOME OF THE SHAPES TO CREATE OPENNESS.
          gp = grid::gpar(col = color, fill = fill, lwd = strokewidth)
        )
      },
      data$shape,
      size * ggplot2::.pt + stroke_size * ggplot2::.stroke / 2,
      ggplot2::alpha(data$colour, data$alpha),
      ggplot2::fill_alpha(data$fill, data$alpha),
      data$stroke * ggplot2::.stroke / 2
    )

    do.call(grid::grobTree, c(g, list(name = "geom_point_plus_key")))
  },

  ##LASTLY WE OVERRIDE DEFAULT_AES SO THAT GGPLOT2 KNOWS WHAT AESTHETICS THIS PROTO ACCEPTS AND WHAT TO SET THEM TO IF THEY ARE NOT AUTOMATICALLY PROVIDED.
  default_aes = ggplot2::aes(
    shape = 21,
    colour = "black",
    fill = NA,
    size = 4,
    alpha = 1,
    stroke = 1.1
  )
)


# Defining custom shapes via series of coordinates ------------------------


#EVERY SHAPE ADDED IS A SERIES OF X/Y COORDINATES TO BE CONNECTED VIA A PATH. EVERYTHING  IS CENTERED AROUND 0,0 AND SHOULD BE VALUES BETWEEN ~ -1 AND 1. PIECE CORRESPONDS TO WHICH ELEMENTS WILL BE CONSIDERED A "HOLE" INSIDE OF OTHER ELEMENTS, WITH ALL ELEMENTS HAVING A PIECE VALUE OTHER THAN 1 BECOMING HOLES.

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

  arm_angles = seq(0, 2*pi - pi/3, length.out = 6)

  #MAKING THIS SHAPE IS A BIT MORE INVOLVED AND REQUIRES THE SF PACKAGE TO CREATE SOME FAKE UNIFIED GEOMETRIES.
  #THE CENTER CIRCLE
  hub      = sf::st_buffer(sf::st_point(c(0, 0)),
                            dist = r_hub, nQuadSegs = nSeg)
  #THE SPOKE CIRCLES
  tip_sf   = lapply(arm_angles, function(a) {
    sf::st_buffer(sf::st_point(c(offset*cos(a), offset*sin(a))),
              dist = r_tip, nQuadSegs = nSeg)
  }) |>
    sf::st_sfc()

  hub_sf = sf::st_sfc(hub)

  #UNIFY THESE TOGETHER
  all_circles = c(hub_sf, tip_sf)

  asterisk_union = sf::st_union(all_circles)

  #THEN EXTRACT JUST THE EXTERIOR COORDINATES OF THE UNION.
  flower = sf::st_coordinates(asterisk_union) |>
    dplyr::as_tibble() |>
    dplyr::mutate(piece = L1) |>
    dplyr::transmute(x = X, y = Y, piece)
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
#' This named list contains elements with x and y coordinates that, when corrected, will draw new shapes to be used as points in `ggplot2` scatterplots. The piece attribute of each element dictates whether a region of the resulting shape will be solid or a "hole" in the final shape.
#'
#' @return A named list.
#' @export
shapes.list = list(
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

#' Create and add a scatterplot layer to your `ggplot2` graph with new, distinctive shapes.
#'
#' This function behaves similarly to `ggplot2::geom_point()` except that it takes several new inputs: `shapes`, `n_shapes`, `shape_values`, `legend_title`, and `key_size`. These are explained below. Collectively, these inputs allow `geom_point_plus()` to access and draw several new and distinctive shapes that are designed to be more readily distinguishable from one another when shape communicates difference.
#'
#' @param mapping Set of aesthetic mappings created by aes(), as in `ggplot2::geom_point()`.
#' @param data The data to be displayed in this layer, as in `ggplot2::geom_point()`.
#' @param stat The statistical transformation to use on the data for this layer, as in `ggplot2::geom_point()`.
#' @param position A position adjustment to use on the data for this layer, as in `ggplot2::geom_point()`.
#' @param shapes A named list of custom shapes to be drawn in place of `ggplot2`'s standard palette of shapes. Defaults to `shapes.list`, the palette of shapes designed specifically for use in `geom_point_plus()` and should (probably) not be changed unless users have created new shapes they would like to use instead.
#' @param n_shapes A length-1 integer corresponding to the number of distinct shapes the function is allowed to pull from the shapes palette specified to `shapes`. Defaults to the length of `shapes` and should (probably) not be changed.
#' @param shape_values A character string referring by name to elements in the `shapes.list` the function should use to allocate shapes to values, e.g. `c("flower", "octagon", "squircle)`. These are provided internally to a `scale_shape_manual()` call and are meant to circumvent the need for such a call to specify a specific subset of shapes to be used. Defaults to `NULL`, i.e., shapes are pulled from `shapes.list` in order.
#' @param legend_title A length-1 character string corresponding to the name to be used for the shape legend title (if any). This is passed internally to `scale_shape_manual()` and is meant to help circumvent the need for the user to specify any such call directly.
#' @param key_size A length-1 numeric value corresponding to the desired size of the legend keys. Defaults to 10. This is passed internally to `scale_shape_manual()` and is meant to help circumvent the need for the user to specify any such call directly.
#' @param include_shape_legend Logical indicating whether a shape legend will be shown (one is always shown unless this is set to FALSE).
#' @param ... Other arguments passed on to layer()'s params argument, as in `ggplot2::geom_point()`.
#' @param na.rm Logical value controlling whether missing values should be removed from the data with a warning or silently, as in `ggplot2::geom_point()`.
#' @param show.legend Logical value controlling whether this layer should be included in the legends, as in `ggplot2::geom_point()`.
#' @param inherit.aes Logical for whether the default aesthetics should be overridden rather than combined with the provided aesthetics, as in `ggplot2::geom_point()`.
#' @return A ggplot2 layer object.
#' @examples
#' ggplot(mtcars, aes(wt, mpg, fill = drat)) + geom_point_plus(aes(shape = factor(gear)), size = 5)
#' ggplot(mtcars, aes(wt, mpg, fill = factor(cyl))) + geom_point_plus(aes(shape = factor(carb)), shape_values = c("squircle", "lotus", "sunburst", "octagon", "cross", "oval"), size = 5, stroke = 0.4)
#' ggplot(iris, aes(Petal.Width, Petal.Length, fill = Species)) + geom_point_plus(aes(shape = Species), size = 5, alpha = 0.7)
#'
#' @export
geom_point_plus = function(mapping = NULL,
                           data = NULL,
                           stat = "identity",
                           position = "identity",
                           shapes = shapes.list, #A NAMED LIST OF SHAPES. DEFAULTS TO THOSE PROVIDED BY ggplot.plus.
                           n_shapes = length(shapes), #HOW MANY DISTINCT SHAPES SHOULD BE PULLED FROM THE AVAILABLE PALETTE? DEFAULTS TO ALL OF THEM.
                           shape_values = NULL, #WE PROVIDE DIRECT ACCESS TO THE VALUES ARGUMENT OF SCALE_SHAPE_MANUAL VIA THE SHAPE_VALUES PARAMETER. THIS WAY, A USER NEEDN'T TACK ON AN ADDITIONAL CALL TO SCALE_SHAPE_MANUAL() TO CUSTOMIZE THE SHAPES USED.
                           legend_title = NULL, #WE ALSO PROVIDE DIRECT ACCESS TO THE TITLE ARGUMENT OF THE LEGEND, AS CHANGING THIS MANUALLY WOULD OTHERWISE REQUIRE ANOTHER CALL TO SCALE_SHAPE_DISCRETE AND THAT WOULD TRIGGER A WARNING AND RESET TO THE SHAPES PALETTE GGPLOT2 GENERALLY USES.
                           key_size = 10, #WE PROVIDE DIRECT ACCESS TO THE SIZES OF THE KEYS IN THE LEGEND TOO.
                           include_shape_legend = TRUE, #WE PROVIDE DIRECT ACCESS TO WHETHER OR NOT A SHAPE LEGEND GETS SHOWN, FOR USE IN SINGLE-SHAPE SCATTERPLOTS WHERE THE CUSTOM SHAPES ARE USED INSTEAD OF GGPLOT2 DEFAULTS.
                           ...,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {

  #THIS FUNCTION IS MOSTLY JUST A WRAPPER TO GEOM_POINT2 INTERNALLY.
  geom_call = geom_point2(
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    shapes = shapes,
    na.rm = na.rm,
    show.legend = if(include_shape_legend == TRUE) show.legend else c(shape = FALSE),
    inherit.aes = inherit.aes,
    ...
  )

  #HOWEVER, SO LONG AS SHAPE HAS BEEN MAPPED, WE FIGURE OUT WHICH AND HOW MANY SHAPES TO PULL FROM THE AVAILABLE SHAPES PALETTE.
  values = shape_values %||% names(shapes)[seq_len(min(length(shapes), n_shapes))]

  #BUILD THE LEGEND IF WE'RE GOING TO, BUT ONLY PUT IN TITLE IF THE USER PROVIDED ONE.
    if (!is.null(legend_title)) {
      scale_call = ggplot2::scale_shape_manual(
        legend_title,
        values = values,
        guide  = ggplot2::guide_legend(override.aes = list(size = key_size))
      )
      return(list(geom_call, scale_call))
    }

    scale_call = ggplot2::scale_shape_manual(
        values = values,
        guide  = ggplot2::guide_legend(override.aes = list(size = key_size))
    )
    return(list(geom_call, scale_call))

}

#' A new palette of shapes available to `geom_point_plus`
#'
#' Call this object to generate a `ggplot` showing the names and features of each of the shapes available to `ggplot.plus::geom_point_plus`.
#'
#' @return A named list.
#' @export
geom_point_plus_shapes = ggplot2::ggplot(data = data.frame(x = rep(c(0.5,1.5,2.5), each = 3),
                         y = rep(c(1,2,3), times = 3),
                         shape = factor(1:9))) +
  geom_point_plus(ggplot2::aes(x = x, y = y, shape = shape, fill = shape), shape_values = c("squircle", "octagon", "flower", "economy", "cross", "waffle", "oval", "sunburst", "lotus"),
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
