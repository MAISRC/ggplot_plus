# Defining custom shapes via series of coordinates ------------------------

#NOTE--THIS CODE REQUIRES POLYCLIP TO RENDER THE FLOWER SHAPE CORRECTLY.

#EVERY SHAPE ADDED IS A SERIES OF X/Y COORDINATES TO BE CONNECTED VIA A PATH. EVERYTHING  IS CENTERED AROUND 0,0 AND SHOULD BE VALUES BETWEEN ~ -1 AND 1, WITH A ROUGH DIAMETER OF 0.4. PIECE CORRESPONDS TO WHICH ELEMENTS WILL BE CONSIDERED A "HOLE" INSIDE OF OTHER ELEMENTS, WITH ALL ELEMENTS HAVING A PIECE VALUE OTHER THAN 1 BECOMING HOLES.

#SHAPE 1: WAFFLE. THIS SHAPE IS CLOSED, INTERSECTIONAL, AND SPIKY.
{
  waffle = data.frame(
    x = c(-5, -5, 5, 5, -3, -1, -1, -3, -3, -1, -1, -3, 1, 3, 3, 1, 1, 3, 3, 1) / 12,
    y = c(-5, 5, 5, -5, -3, -3, -1, -1, 1, 1, 3, 3, -3, -3, -1, -1, 1, 1, 3, 3) / 12,
    piece = rep(1:5, each = 4)
  )
}

#SHAPE 2: PLUS THIS SHAPE IS INTERMEDIATE IN OPENNESS, INTERSECTIONALITY, AND SPIKINESS.
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

  plus = data.frame(x = xy[,1],
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

#SHAPES 10-15 ARE THE ORIGINAL SHAPES AVAILABLE IN VANILLA R (PCH 21-25)
circle = data.frame(
  x = cos(seq(0, 2*pi, length.out = 50)) * 0.4,
  y = sin(seq(0, 2*pi, length.out = 50)) * 0.4,
  piece = 1
)

square = data.frame(
  x = c(-0.4, -0.4,  0.4,  0.4),
  y = c(-0.4,  0.4,  0.4, -0.4),
  piece = 1
)

diamond = data.frame(
  x = c( 0.0,  0.4,  0.0, -0.4),
  y = c( 0.4,  0.0, -0.4,  0.0),
  piece = 1
)

triangle_up = data.frame(
  x = c( 0.0, -0.4,  0.4),
  y = c( 0.4, -0.4, -0.4),
  piece = 1
)

triangle_down = data.frame(
  x = c( 0.0, -0.4,  0.4),
  y = c(-0.4,  0.4,  0.4),
  piece = 1
)

#BUNDLES ALL THAT UP INTO AN OBJECT.
ggplotplus_shapes_list = list(
  plus = plus,
  octagon = octagon,
  flower = flower,
  oval = oval,
  waffle = waffle,
  squircle = squircle,
  sunburst = sunburst,
  lotus = egg_of_life,
  economy = economy,
  circle = circle,
  square = square,
  diamond = diamond,
  triangle_up = triangle_up,
  triangle_down = triangle_down
)

#BUNDLES THIS INTO A STORED DATA OBJECT WHICH WILL BE FASTER TO LOAD AT INSTALL.
usethis::use_data(
  ggplotplus_shapes_list,
  overwrite = TRUE,
  compress = "gzip",
  version = 2
)
