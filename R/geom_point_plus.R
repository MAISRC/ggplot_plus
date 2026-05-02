# Backend functions -------------------------------------------------------

#' A backend version of `geom_point()` that can access distinctive shapes.
#'
#' This function behaves much like `ggplot2`'s `geom_point()` function except that it allows access to a palette of nine new shapes that vary in their openness, spikiness, and intersectionality, making them more easily distinguished. This function is not meant to be called directly--instead, `geom_point_plus()` calls and modifies this function and is the intended function for users.
#'
#' @param mapping Set of aesthetic mappings created by aes(), as in `ggplot2::geom_point()`.
#' @param data The data to be displayed in this layer, as in `ggplot2::geom_point()`.
#' @param stat The statistical transformation to use on the data for this layer, as in `ggplot2::geom_point()`.
#' @param position A position adjustment to use on the data for this layer, as in `ggplot2::geom_point()`.
#' @param shapes A named list of custom shapes to be drawn in place of `ggplot2`'s standard palette of shapes.
#' @param ... Other arguments passed on to layer()'s params argument, as in `ggplot2::geom_point()`.
#' @param na.rm Logical controlling whether missing values should be removed from the data with a warning or silently, as in `ggplot2::geom_point()`.
#' @param show.legend Logical controlling whether this layer should be included in the legend(s), as in `ggplot2::geom_point()`.
#' @param inherit.aes Logical controlling whether global aesthetics specified in `ggplot2::ggplot()` should be inherited locally by this layer or not, as in `ggplot2::geom_point()`.
#' @return A ggplot2 layer object.
#' @keywords internal
geom_point2 = function(mapping = NULL,
                       data = NULL,
                       stat = "identity",
                       position = "identity",
                       shapes = ggplotplus_shapes_list, #THE KEY INPUT--THIS IS THE NEW PALETTE OF SHAPES AVAILABLE.
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
#' This ggplot proto object is called internally by `geom_point2()` and inherits most, but not all, of its methods and properties from `ggplot2`'s standard `geomPoint` proto. However, it has different default aesthetics, a different shapes palette, and can draw these new shapes in a legend. This subclass is not meant to be encountered by the user and is instead fodder for `geom_point_plus()`.
#'
#' @return A `ggplot2` ggproto subclass object.
#' @keywords internal
GeomPointPlus = ggplot2::ggproto(
  "PointPlus",
  ggplot2::GeomPoint, #THIS NEW GEOM PROTO INHERITS PROPERTIES AND METHODS FROM GEOMPOINT, BUT WE OVERRIDE TWO: DRAW_PANEL AND DRAW_KEY. IT BEHAVES EXACTLY LIKE GEOM_POINT UNLESS A CHARACTER-BASED SHAPE INPUT IS PROVIDED THAT MATCHES ONE IN THE CUSTOM SHAPES LIST.

  #DRAW PANEL IS THE METHOD THAT DICTATES WHAT EXACTLY GETS DRAWN IN THE PLOT PANEL. WE OVERRIDE IT SO OUR CUSTOM SHAPES COME THROUGH.
  draw_panel = function(self,
                        data,
                        panel_params,
                        coord,
                        shapes,
                        na.rm = FALSE) {

    if(is.factor(data$shape)) { data$shape = as.character(data$shape) } #COERCE FACTORS, WHICH MIGHT LOOK NUMERIC, TO CHARACTERS SO CUSTOM SHAPE STRINGS WORK.

    coords = coord$transform(data, panel_params) #TRANSFORM DATA INTO THE PANEL COORDINATE SYSTEM FIRST.

    stroke_size = coords$stroke #NORMALIZE STROKE SIZE.
    stroke_size[is.na(stroke_size)] = 0

    #TWO DIFFERENT WAYS OF INPUTTING SHAPES MEANS TWO DIFFERENT WAYS OF ADDRESSING THE INPUTS. IF THE SHAPE DATA ARE NUMERIC, THE USER PRESUMABLY WANTS PLAIN-OLD GGPLOT2 POINTS AND WE DELEGATE THAT TO THE STANDARD POINTS GROB.

    if(!is.character(coords$shape)) {
      return(grid::pointsGrob(
        x   = coords$x,
        y   = coords$y,
        pch = coords$shape,
        gp  = grid::gpar(
          col       = ggplot2::alpha(coords$colour, coords$alpha),
          fill      = ggplot2::fill_alpha(coords$fill, coords$alpha),
          fontsize  = coords$size * ggplot2::.pt + stroke_size * ggplot2::.stroke / 2,
          lwd       = stroke_size * ggplot2::.stroke / 2
        )
      ))
    }

    #OTHERWISE, THE METHOD ASSUMES YOU WANT TO USE THE CUSTOM SHAPES, WHICH IT'LL DRAW BY CONNECTING A SERIES OF COORDINATES USING A PATHGROB.
    #EVERY SHAPE INPUT NEEDS TO BE IN THE SHAPES LIST, AND IT NEEDS TO BE A NAMED LIST WITH $X $Y AND $PIECE ELEMENTS. THE X AND Y VALUES SHOULD BE CENTERED AROUND 0,0 AND BE SCALED TO ~0.4 RADIUS TO BE OF SIMILAR SIZES TO EACH OTHER.

    #FIRST, WE MAKE SURE WE HAVE ALL THE REQUIRED DATA AND OTHERWISE RETURN NULLGROB.
    ok = is.finite(coords$x) &
      is.finite(coords$y) &
      !is.na(coords$shape)
    if(!any(ok)) { return(grid::nullGrob()) }

    coords = coords[ok, , drop = FALSE] #GET RID OF ALL NAs SO WE NEVER ERROR OUT TRYING TO USE THEM AS REFERENTS.

    #IF A USER SPECIFIED AN UNKNOWN SHAPE, BAIL EARLY WITH USEFUL MESSAGE.
    unknown = setdiff(unique(coords$shape),
                      names(shapes))
    if(length(unknown)) {
      stop(sprintf(
        "Unknown shape name(s): %s. Valid names are: %s",
        paste(unknown, collapse = ", "),
        paste(names(shapes), collapse = ", ")
      ), call. = FALSE)
    }

    #PRECOMPUTE THE POINT SIZING IN PTS, REFERRING ONLY TO NON-NA POINTS.
    size_pts = coords$size * ggplot2::.pt + stroke_size * ggplot2::.stroke / 2
    stroke_pts = stroke_size * ggplot2::.stroke / 2

    .pointplus_scale = 1.3  #TWEAK AS NEEDED TO GET POINTS TO BE ROUGHLY THE RIGHT SIZE.

    #BUILD ONE PATH GROB (CUSTOM SHAPE) PER POINT, USING THE EVEN-ODD RULE TO PUNCH HOLES OUT OF THE CENTER OF SOME SHAPES.
    g = Map(function(x, y, shape, size, color, fill, lwd) {
      dat  = shapes[[shape]]
      xval = unit(x, "npc") + unit(dat$x * size * .pointplus_scale, "points")
      yval = unit(y, "npc") + unit(dat$y * size * .pointplus_scale, "points")
      grid::pathGrob(
        x = xval, y = yval, id = dat$piece,
        rule = "evenodd",
        gp = grid::gpar(
          col = color,
          fill = fill,
          lwd = lwd
        )
      )
    },
    coords$x, coords$y, coords$shape,
    size_pts,
    ggplot2::alpha(coords$colour, coords$alpha),
    ggplot2::fill_alpha(coords$fill, coords$alpha),
    stroke_pts)

    #ADD POINTS LAYER TO THE GRAPH'S GROBTREE.
    grid::grobTree(grobs = do.call(grid::gList, g), name = "geom_point_plus")
  },

  #DRAW_KEY IS THE METHOD USED TO DRAW THE SYMBOLS IN THE LEGENDS. IT FOLLOWS THE SAME BEHAVIOR AS DRAW_PANEL FOR THE MOST PART EXCEPT IT ONLY DRAWS A SINGLE SYMBOL PER SHAPE NEEDED AT COORDINATES OF 0.5, 0.5 IN NPC UNITS.
  draw_key = function (data, params, size) {

    #SET DEFAULTS THAT MIRROR WHAT THE PACKAGE USES FOR GEOM_POINT. PROBABLY NOT NECESSARY BUT ALSO PROBALBY NOT HURTING ANYTHING.
    if(is.null(data$shape)) data$shape = 21
    stroke_size = data$stroke %||% 1.2
    stroke_size[is.na(stroke_size)] = 0
    size = data$size %||% 5


    #NUMERIC SHAPE VALUES ONCE AGAIN DIVERT TO STANDARD POINTSGROB.
    if(!is.character(data$shape)) {
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


    #OTHERWISE WE DRAW THE KEYS USING OUR CUSTOM SHAPES.
    size_pts   = size * ggplot2::.pt + stroke_size * ggplot2::.stroke / 2
    lwd_pts    = stroke_size * ggplot2::.stroke / 2

    shapes_vec = as.character(data$shape)
    g = Map(function(shape, color, fill) {
      dat  = params$shapes[[shape]]
      xval = grid::unit(0.5, "npc") + grid::unit(dat$x * size_pts, "points")
      yval = grid::unit(0.5, "npc") + grid::unit(dat$y * size_pts, "points")
      grid::pathGrob(
        x = xval, y = yval, id = dat$piece,
        rule = "evenodd",
        gp = grid::gpar(col = color, fill = fill, lwd = lwd_pts)
      )
    },
    shapes_vec,
    ggplot2::alpha(data$colour, data$alpha),
    ggplot2::fill_alpha(data$fill, data$alpha))

    #AND ADD TO THE PLOT'S GROB TREE.
    grid::grobTree(grobs = do.call(grid::gList, g), name = "geom_point_plus_key")
  }
)
