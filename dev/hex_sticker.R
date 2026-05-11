library(ggplot2)
library(hexSticker)
library(magick)

shape_grob = function(dat,
                      fill,
                      colour = "#333",
                      lwd = 0.35,
                      xlim = c(-1.35, 1.35),
                      ylim = c(-1.35, 1.35)) {

  x_npc = (dat$x - xlim[1]) / diff(xlim)
  y_npc = (dat$y - ylim[1]) / diff(ylim)

  grid::pathGrob(
    x = grid::unit(x_npc, "npc"),
    y = grid::unit(y_npc, "npc"),
    id = dat$piece,
    rule = "evenodd",
    gp = grid::gpar(
      fill = fill,
      col = colour,
      lwd = lwd
    )
  )
}

viridis_colors = viridis(9, begin = 0, end = 1)
viridis_teal = viridis_colors[5]
viridis_remaining = viridis_colors[-5]

#RESCALES SHAPES TO DIFFERENT SIZES.
scale_shape = function(dat, s = 1, x0 = 0, y0 = 0) {
  transform(dat, x = x * s + x0, y = y * s + y0)
}

# example orbit using other package shapes
orbit_names = c("flower", "lotus", "waffle", "octagon", "squircle", "sunburst", "oval", "economy")



# central plus:
plus_big = scale_shape(ggplotplus_shapes_list$plus, s = 1.4)

#MAKE ALL THE OTHER SHAPES SMALLER.
orbit = do.call(rbind, lapply(seq_along(orbit_names), function(i) {

  theta = 2*pi * (i - 1) / length(orbit_names)

  dat = ggplotplus_shapes_list[[orbit_names[i]]]

  dat = scale_shape(
    dat,
    s = 0.35,
    x0 = cos(theta) * 1,
    y0 = sin(theta) * 1
  )

  dat$fill = viridis_remaining[(i - 1) %% length(viridis_remaining) + 1]
  dat$shape_id = orbit_names[i]
  dat
}))

logo_plot = ggplot() +
  coord_equal(
    xlim = c(-1.35, 1.35),
    ylim = c(-1.35, 1.35),
    clip = "off"
  ) +
  theme_void()

for(i in seq_along(orbit_names)) {

  this_shape = orbit[orbit$shape_id == orbit_names[i], ]

  logo_plot = logo_plot +
    annotation_custom(
      grob = shape_grob(
        dat = this_shape,
        colour = "#222",
        lwd = 0.75,
        fill = unique(this_shape$fill)
      ),
      xmin = -Inf, xmax = Inf,
      ymin = -Inf, ymax = Inf
    )
}

plus_big$piece = as.integer(plus_big$piece)

logo_plot = logo_plot +
  annotation_custom(
    grob = shape_grob(
      dat = plus_big,
      fill = viridis_teal,
      colour = "#222",
      lwd = 1.75
    ),
    xmin = -Inf, xmax = Inf,
    ymin = -Inf, ymax = Inf
  )

hexSticker::sticker(
  subplot = logo_plot,
  package = "",
  filename = "man/figures/ggplotplus_hex.png",

  s_x = 1,
  s_y = 0.75,
  s_width = 1.25,
  s_height = 1.25,

  h_fill = "#FFFCDE",
  h_color = ink,

  dpi = 600
)


img = image_read("man/figures/ggplotplus_hex.png")

img = image_annotate(
  img,
  "ggplot",
  size = 100,
  color = ink,
  gravity = "north",
  location = "+-150+235",
  font = "Arial"
)

img = image_annotate(
  img,
  "plus",
  size = 150,
  color = "#440154",
  gravity = "north",
  location = "+125+200",
  font = "Arial Bold"
)

image_write(img, "man/figures/ggplotplus_hex.png")
