#ENHANCED VERSIONS OF EACH COMMON GEOM SO AS TO HAVE IMPROVED DEFAULTS (THAT ARE STILL OVERRIDABLE)

#' @importFrom ggplot2 ggplot_add
NULL

#' Default settings for geometry layers created by `geom_plus()`
#'
#' A named list of default aesthetics used by `geom_plus()` to control styling of the resulting geometry layers.
#'
#' @format A named list with elements like "point", "jitter", "boxplot", etc., corresponding to commonly used ggplot2 geometries. USe names(geom_plus_defaults) for a full list.
#' @export
geom_plus_defaults = list(
  point = list(size = 5,
               shape = 21,
               stroke = 1.2,
               fill = "transparent",
               colour = "black"),
  jitter = list(size = 5,
                shape = 21,
                fill = "transparent",
                colour = "black",
                stroke = 1.2),
  count = list(size = 5,
               shape = 21,
               fill = "transparent",
               colour = "black",
               stroke = 1.2),
  boxplot = list(linewidth = 1.2,
                 staplewidth = 0.25,
                 outlier.size = 5,
                 outlier.shape = 21,
                 outlier.colour = "black",
                 outlier.stroke = 1.2,
                 outlier.fill = "transparent"),
  violin = list(linewidth = 1.2,
                linetype = "solid",
                colour = "black",
                fill = "white",
                draw_quantiles = c(0.25, 0.5, 0.75)), #THIS WAY, THE MEDIAN AND Q1/Q3 ARE SHOWN BY DEFAULT.
  bar = list(fill = "transparent",
             colour = "black",
             linewidth = 1.2),
  col = list(fill = "transparent",
             colour = "black",
             linewidth = 1.2),
  histogram = list(fill = "transparent",
                   colour = "black",
                   linewidth = 1.2,
                   linetype = "solid"),
  line = list(linewidth = 1.5, #MAKE IT THICKER STILL TO POP OUT AGAINST OTHER LINE ELEMENTS.
              colour = "black",
              linetype = "solid",
              alpha = 1),
  freqpoly = list(linewidth = 1.2,
                  colour = "black",
                  linetype = "solid",
                  alpha = 1),
  segment = list(linewidth = 1.2,
                 colour = "black",
                 linetype = "solid",
                 alpha = 1),
  abline = list(linewidth = 1.2,
                colour = "black",
                linetype = "solid",
                alpha = 1),
  hline = list(linewidth = 1.2,
               colour = "black",
               linetype = "solid",
               alpha = 1),
  vline = list(linewidth = 1.2,
               colour = "black",
               linetype = "solid",
               alpha = 1),
  curve = list(linewidth = 1.2,
               colour = "black",
               linetype = "solid",
               alpha = 1),
  smooth = list(linewidth = 1.2,
                colour = "black",
                fill = "black",
                linetype = "solid",
                alpha = 0.3),
  area = list(linewidth = 1.2,
              linetype = "solid",
              colour = "black",
              fill = "black",
              alpha = 0.3),
  ribbon = list(linewidth = 1.2,
                linetype = "solid",
                colour = "black",
                fill = "black",
                alpha = 0.3),
  crossbar = list(linewidth = 1.2,
                  linetype = "solid",
                  colour = "black"),
  errorbar = list(linewidth = 1.2,
                  linetype = "solid",
                  colour = "black"),
  linerange = list(linewidth = 1.2,
                   linetype = "solid",
                   colour = "black"),
  pointrange = list(linewidth = 1.2,
                    linetype = "solid",
                    colour = "black"),
  density = list(linewidth = 1.2,
                 colour = "black",
                 fill = "black",
                 linetype = "solid",
                 alpha = 0.3),
  dotplot = list(stroke = 1.2,
                 colour = "black",
                 fill = "transparent",
                 linetype = "solid"),
  tile = list(alpha = 1, #DON'T WEAKEN CONTRAST AGAINST BACKGROUND
              height = 0.85, #TO AVOID SIMULTANEOUS CONTRAST ILLUSION CAUSED BY COMPARING NEIGHBORING COLORS RELATIVELY TO EACH OTHER RATHER THAN TO AN IMMEDIATE ABSOLUTE. THIS CAUSES BLEEDTHROUGH OF THE BACKGROUND BETWEEN TILES.
              width = 0.85)
)


#' Generates Base Geoms With Elevated Defaults
#'
#' Maps inputs to a base ggplot geom (e.g., geom_point or geom_line) but provides default values more likely to adhere to best practices around usability, design aesthetics, and accessibility.
#'
#' @param geom The name of the geom being drawn. Corresponds to the portion of the geom_ function after the _, e.g., "point" for geom_point, "line" for geom_line, etc. Must be a length one character string, and must match an implemented geom. See names(geom_plus_defaults) for a list of these. Required input.
#' @param ... Other arguments to be passed along to the geom_ function being called.
#' @param include_theme Should a call to `theme_plus()` with no arguments be automatically applied to the ggplot command chain, without needing to be called separately? Defaults to `FALSE`. Set to `TRUE` to include it.
#' @param include_gridlines Should a call to `gridlines_plus()` with no arguments be automatically applied to the ggplot command chain, without needing to be called separately? Defaults to `FALSE`. Set to `TRUE` to include it.
#' @param include_xscale_plus Should a call to `scale_x_continuous_plus()` with no arguments be automatically applied to the ggplot command chain, without needing to be called separately? Defaults to `FALSE`. Set to `TRUE` to include it.
#' @param include_yscale_plus Should a call to `scale_y_continuous_plus()` with no arguments be automatically applied to the ggplot command chain, without needing to be called separately? Defaults to `FALSE`. Set to `TRUE` to include it.
#' @param include_fillscale_plus Should a call to `scale_fill_continuous_plus()` with no arguments be automatically applied to the ggplot command chain, without needing to be called separately? Defaults to `FALSE`. Set to `TRUE` to include it.
#' @param include_colorscale_plus Should a call to `scale_color_continuous_plus()` with no arguments be automatically applied to the ggplot command chain, without needing to be called separately? Defaults to `FALSE`. Set to `TRUE` to include it.
#' @param new_x_title A string to use for the graph's x axis title. Defaults to `NULL` and will be ignored unless a length-1 string.
#' @param new_y_title A string to use for the graph's y axis title. Defaults to `NULL` and will be ignored unless a length-1 string.
#' #' @param new_fill_title A string to use for the graph's fill legend title. Defaults to `NULL` and will be ignored unless a length-1 string.
#' @param new_color_title A string to use for the graph's color legend title. Defaults to `NULL` and will be ignored unless a length-1 string.
#' @param new_size_title A string to use for the graph's size legend title. Defaults to `NULL` and will be ignored unless a length-1 string.
#' @param new_shape_title A string to use for the graph's shape legend title. Defaults to `NULL` and will be ignored unless a length-1 string.
#' @param new_alpha_title A string to use for the graph's alpha legend title. Defaults to `NULL` and will be ignored unless a length-1 string.
#' @param silence_warnings `geom_plus()` triggers some checks for aspects of good graph design and, if any of these checks fail, a warning is triggered to direct the user towards better practices. Set this parameter to `FALSE` to silence these warnings.
#' @return List with the class "geom_plus", which will trigger the geom_plus method in ggplot_add.
#' @examples
#' ggplot(iris, aes(x=Sepal.Length, y=Petal.Length)) + geom_plus(geom = "point")
#' @export
geom_plus = function(geom, ...,
                    include_theme = FALSE,
                    include_gridlines = FALSE,
                    include_xscale_plus = FALSE,
                    include_yscale_plus = FALSE,
                    include_fillscale_plus = FALSE,
                    include_colorscale_plus = FALSE,
                    new_x_title = NULL,
                    new_y_title = NULL,
                    new_color_title = NULL,
                    new_fill_title = NULL,
                    new_size_title = NULL,
                    new_shape_title = NULL,
                    new_alpha_title = NULL,
                    silence_warnings = FALSE) {
  if(is.null(geom)) { stop("A geom must be specified!") } #MUST SPECIFY A GEOM
  if(!geom %in% names(geom_plus_defaults)) { stop("The geom you've specified either doesn't exist or hasn't been implemented yet. Please double-check your code for typos. Note that this function only needs the part following the _ in the geom's name. For a complete list of implemented geoms, run names(geom_plus_defaults). ") }

  structure(
    list(geom = geom,
         include_theme = include_theme,
         silence_warnings = silence_warnings,
         include_gridlines = include_gridlines,
         include_xscale_plus = include_xscale_plus,
         include_yscale_plus = include_yscale_plus,
         include_fillscale_plus = include_fillscale_plus,
         include_colorscale_plus = include_colorscale_plus,
         new_x_title = new_x_title,
         new_y_title = new_y_title,
         new_fill_title = new_fill_title,
         new_color_title = new_color_title,
         new_size_title = new_size_title,
         new_shape_title = new_shape_title,
         new_alpha_title = new_alpha_title,
         user_args = list(...)),
    class = "geom_plus"
  )
}

#' Add A geom_plus-generated Geometry to a ggplot
#'
#' This method defines how objects of class `geom_plus()`, added by the geom_plus function, are added to a ggplot2 plot using the `+` operator.
#' It processes default aesthetics, handles user overrides, and ensures compatibility with ggplot2 layering.
#'
#' @param object An object of class `geom_plus`, created by `geom_plus()`, containing user-provided arguments (if any).
#' @param plot A ggplot object to which the new geometry layer should be added.
#' @param name Internal name used by ggplot2 when adding the layer.
#'
#' @return A ggplot object with the new geometry layer added.
#' @export
ggplot_add.geom_plus = function(object, plot, name) {

  user_args = object$user_args #LOCALLY PROVIDED USER ARGUMENTS.
  geom_name = object$geom #UNPACK THE GEOM CHOSEN
  silence_warnings = object$silence_warnings #UNPACK USER DESIRES FOR WARNINGS.
  include_theme = object$include_theme #UNPACK USER DESIRES FOR THEME.
  include_gridlines = object$include_gridlines #UNPACK USER DESIRES AROUND GRIDLINES.
  include_xscale_plus = object$include_xscale_plus #UNPACK USER DESIRES AROUND X SCALE ADJUSTMENT.
  include_yscale_plus = object$include_yscale_plus #UNPACK USER DESIRES AROUND Y SCALE ADJUSTMENT.
  include_fillscale_plus = object$include_fillscale_plus #UNPACK USER DESIRES AROUND FILL SCALE ADJUSTMENT.
  include_colorscale_plus = object$include_colorscale_plus #UNPACK USER DESIRES AROUND COLOR SCALE ADJUSTMENT.
  new_x_title = object$new_x_title
  new_y_title = object$new_y_title
  new_fill_title = object$new_fill_title
  new_color_title = object$new_color_title
  new_size_title = object$new_size_title
  new_shape_title = object$new_shape_title
  new_alpha_title = object$new_alpha_title

  #IF THE USER PROVIDED LOCAL MAPPINGS, WE GRAB THOSE HERE.
  aes_local = NULL #STORAGE OBJ.

  #THEY MAY NOT HAVE NAMED THE MAPPINGS EXPLICITLY. IF NOT, DO THIS.
  if (length(user_args) > 0 &&
      inherits(user_args[[1]], "uneval")) {
    aes_local = user_args[[1]]
    user_args[[1]] = NULL
    #OTHERWISE, WE FIND THE MAPPINGS BY THEIR NAME.
  } else if (!is.null(user_args$mapping)) {
    aes_local = user_args$mapping
    user_args$mapping = NULL
  }
  #NOW, aes_local IS THE LOCAL MAPPINGS HOWEVER THEY WERE SPECIFIED.

  #NEXT, EXTRACT THE GLOBAL MAPPINGS.
  aes_global = plot$mapping

  #GGPLOT FUNCTIONS TAKE BOTH COLOR AND COLOUR AS THE SAME THING, SO WE TRANSLATE ANY "COLOR" STRINGS HERE PRIOR TO MATCHING.
  if(any(names(aes_global) == "color")) { names(aes_global)[which(names(aes_global) == "color")] = "colour" }
  if(any(names(aes_local) == "color")) { names(aes_local)[which(names(aes_local) == "color")] = "colour" }
  if(any(names(user_args) == "color")) { names(user_args)[which(names(user_args) == "color")] = "colour" }
  if(any(names(aes_global) == "outlier.color")) { names(aes_global)[which(names(aes_global) == "outlier.color")] = "outlier.colour" }
  if(any(names(aes_local) == "outlier.color")) { names(aes_local)[which(names(aes_local) == "outlier.color")] = "outlier.colour" }
  if(any(names(user_args) == "outlier.color")) { names(user_args)[which(names(user_args) == "outlier.color")] = "outlier.colour" }

  #IF USERS SPECIFY THE SAME AESTHETIC BOTH INSIDE AND OUTSIDE OF AES LOCALLY, WE WANT TO FLAG THAT WITH A WARNING:
  conflict_aes = intersect(names(aes_local), names(user_args))
  if (length(conflict_aes) > 0) {
    warning("The following aesthetics are mapped and also set to constants inside your geom call. The constant values will be used: ",
            paste(conflict_aes, collapse = ", "))
  }

  #IF USERS SPECIFIED AESTHETICS GLOBALLY, WE KEEP THOSE. IF THEY WERE MAPPED LOCALLY (EITHER NAMED OR UNNAMED), THOSE WIN OUT ONLY IF GLOBAL MAPPINGS AREN'T PROVIDED. THEN, IF AESTHETICS ARE SET TO CONSTANTS, WE GO WITH THOSE IF NEITHER OF THE OTHER TWO OPTIONS ARE USED. OTHERWISE, WE FALL TO THE DEFAULTS.

  defaults = geom_plus_defaults[[geom_name]] #USE THE LOOKUP TABLE TO MATCH THE SPECIFIED GEOM TO THE LIST OF DEFAULTS.

  #WE NOW SET UP A SERIES OF "CONTESTS", WHERE WE HAVE INPUTS LOSE TO HIGHER-PRIORITY INPUTS. THESE ARE DEFAULTS < GLOBAL AES < LOCAL AES < LOCAL CONSTANTS.

  #THE ONLY DEFAULTS WE SHOULD USE ARE FOR THOSE AESTHETICS NOT REFERENCED ANY OTHER TIMES.
  defaults_used = defaults[!names(defaults) %in% c(names(user_args), names(aes_local), names(aes_global))]

  #THE ONLY GLOBAL AESTHETICS WE SHOULD USE ARE THOSE NOT REFERENCE LOCALLY IN ANY WAY.
  #OF COURSE, USERS MIGHT HAVE SPECIFIED ONE THING BUT MEANT ANOTHER. ONE EXAMPLE IS INHERITANCE. IF THEY SPECIFY INHERIT.AES = FALSE, WE NEED TO RESPECT THAT HERE:
  inherit = if(!is.null(user_args$inherit.aes)) { user_args$inherit.aes } else {TRUE}

  if(inherit) {
  globals_used = aes_global[!names(aes_global) %in% c(names(user_args), names(aes_local))]
  } else {
    globals_used = list()
  }

  #WE SHOULD USE ALL LOCAL AESTHETICS UNLESS THE USER ALSO SET THAT SAME AESTHETIC TO A CONSTANT (WEIRD/UNCOMMON)

  locals_used = aes_local[!names(aes_local) %in% c(names(user_args))]

  #WHAT'S LEFT IS ANYTHING SET TO A LOCAL CONSTANT (I THINK), WHICH IS WHAT IS IN USER_ARGS.

  #BEFORE BUNDLING, WE NEED TO CHECK TO MAKE SURE EVERYTHING HERE IS A LIST:
  if(length(globals_used) < 1 || is.null(globals_used)) { globals_used = list() }
  if(length(locals_used) < 1 || is.null(locals_used)) { locals_used = list() }
  if(length(defaults_used) < 1 || is.null(defaults_used)) { defaults_used = list() }
  if(length(user_args) < 1 || is.null(user_args)) { user_args = list() }

  #WE CAN THEN BUNDLE THE DEFAULTS AND USER ARGS AND THE GLOBAL AND LOCAL AESTHETICS
  final_aes = structure(c(globals_used, locals_used), class = "uneval") #NEEDS TO BE A LIST WITH A SPECIFIC CLASS TO LOOK LIKE WHAT AES() MAKES. THIS FORMULATION ALSO PRESERVES NULLS
  final_args = c(defaults_used, user_args)
  #AT THIS POINT, IF ANY REMAINING USER_ARGS ARE NULL, I THINK THEY CAN NOW SAFELY BE REMOVED SO AS TO NOT TRIGGER ERRORS. THEY'VE ALREADY "WON" ANY AESTHETIC CONFLICTS SO A WIPE OF A PREVIOUSLY SET AESTHETIC WILL HAVE HAPPENED. SO THIS IS ESSENTIALLY WIPING THIS AESTHETIC BACK TO A GGPLOT DEFAULT.
  final_args = final_args[!unname(unlist(lapply(final_args, is.null)))]

  use_these_args = c(
    list(
      mapping = final_aes
    ),
    final_args
  )

  #FOR HANDLING SHAPE SPECIFICALLY, WE CHECK TO SEE IF SHAPE HAS BEEN MAPPED.
  if("shape" %in% names(use_these_args$mapping)) {

    shape_var = rlang::as_label(use_these_args$mapping$shape) #IF IT HAS, FIND THE VARIABLE IT'S MAPPED TO.

    #IF THEY SPECIFIED IT LOCALLY IN DATA, WE GRAB IT THERE.
    if(shape_var %in% names(object$user_args$data)) {
      # WE CHECK TO SEE HOW MANY UNIQUE VALS THERE ARE. NEEDS TO BE BETWEEN 1 AND 5.
      shape_num = length(unique(object$user_args$data[[shape_var]]))
      if(!is.null(shape_num) && shape_num > 0 && shape_num < 6) { #IF IT IS.
        shape_pal = c(21:25)[1:shape_num] #WE FIT IT TO THE SHAPE PALETTE OF FILLED SHAPED.
      } else { #OTHERWISE, WE WARN.
        warning("You've mapped shape to a variable with either too few or too many unique values for shape to be an appropriate visual channel, so your shape mapping is not going to be ideal. Use shape for discrete variables with between 1-5 unique values only.")
        shape_pal = c(21:25)[1:shape_num] #WE FIT IT TO THE SHAPE PALETTE OF FILLED SHAPED.
      }
    } else { #OTHERWISE, WE GO LOOKING IN THE GLOBAL PLOT DATA AND SO THE SAME THING.
    if(shape_var %in% names(plot$data)) {
      shape_num = length(unique(plot$data[[shape_var]]))
      if(!is.null(shape_num) && shape_num > 0 && shape_num < 6) {
        shape_pal = c(21:25)[1:shape_num]
      } else {
        warning("You've mapped shape to a variable with either too few or too many unique values for shape to be an appropriate visual channel, so your shape mapping is not going to be ideal. Use shape for discrete variables with between 1-5 unique values only.")
        shape_pal = c(21:25)[1:shape_num] #WE FIT IT TO THE SHAPE PALETTE OF FILLED SHAPED.
      }
     }
    }
  }

  geom_fn = get(paste0("geom_", geom_name), mode = "function") #GET THE ACTUAL FUNCTION THAT THE USER IS HOPING WE'LL USE HERE.
  layer = do.call(geom_fn, use_these_args)

  #WE DO THINGS JUST A LITTLE DIFFERENTLY IF SHAPE WAS MAPPED AND WE HAVE TO ADJUST THE PALETTE.
  if(!exists("shape_pal")) { #THIS IS A SAFE CONSTRUCTION THAT PREVENTS ERRORS WHEN shape_pal IS NULL.
    plot = plot + layer
  } else {
    plot = plot + layer + ggplot2::scale_shape_manual(values = shape_pal)
  }


  ##IN THE INTERESTS OF TRYING TO BUNDLE AS MUCH GGPLOT_PLUS FUNCTIONALITY INTO THE GEOM_PLUS() FUNCTION AS WE CAN, WE'RE INCLUDING TOGGLES THAT WILL JUST APPLY SOME OF THE DEFAULT FUNCTIONS AUTOMATICALLY UNLESS TOGGLED OFF.
  if(include_theme == TRUE) {
    plot = plot + theme_plus() #INCLUDE A VANILLA theme_plus() CALL IF THIS IS TRUE. OTHERWISE OMIT.
  }
  if(include_gridlines == TRUE) {
    plot = plot + gridlines_plus() #INCLUDE A VANILLA gridlines_plus() CALL IF THIS IS TRUE. OTHERWISE OMIT.
  }
  if(include_xscale_plus == TRUE) {
    plot = plot + scale_x_continuous_plus() #INCLUDE A VANILLA scale_x_continuous_plus() CALL IF THIS IS TRUE. OTHERWISE OMIT.
  }
  if(include_yscale_plus == TRUE) {
    plot = plot + scale_y_continuous_plus() #INCLUDE A VANILLA scale_y_continuous_plus() CALL IF THIS IS TRUE. OTHERWISE OMIT.
  }
  if(include_fillscale_plus == TRUE) {
    plot = plot + scale_fill_continuous_plus() #INCLUDE A VANILLA scale_fill_continuous_plus() CALL IF THIS IS TRUE. OTHERWISE OMIT.
  }
  if(include_colorscale_plus == TRUE) {
    plot = plot + scale_color_continuous_plus() #INCLUDE A VANILLA scale_color_continuous_plus() CALL IF THIS IS TRUE. OTHERWISE OMIT.
  }

  #FOR SHORTCUTS TO RENAMING AXIS/LEGEND TITLES:
  if(exists("new_x_title") &&
     is.character(new_x_title) &&
     length(new_x_title) == 1) {
    plot = plot + xlab(new_x_title)
  }
  if(exists("new_y_title") &&
     is.character(new_y_title) &&
     length(new_y_title) == 1) {
    plot = plot + ylab(new_y_title)
  }
  if(exists("new_fill_title") &&
     is.character(new_fill_title) &&
     length(new_fill_title) == 1) {
    plot = plot + labs(fill = new_fill_title)
  }
  if(exists("new_color_title") &&
     is.character(new_color_title) &&
     length(new_color_title) == 1) {
    plot = plot + labs(colour = new_color_title)
  }
  if(exists("new_size_title") &&
     is.character(new_size_title) &&
     length(new_size_title) == 1) {
    plot = plot + labs(size = new_size_title)
  }
  if(exists("new_shape_title") &&
     is.character(new_shape_title) &&
     length(new_shape_title) == 1) {
    plot = plot + labs(shape = new_shape_title)
  }
  if(exists("new_alpha_title") &&
     is.character(new_alpha_title) &&
     length(new_alpha_title) == 1) {
    plot = plot + labs(alpha = new_alpha_title)
  }

  # #IF A BAR OR COLUMN PLOT IS BEING USED, WE SHOULD ELIMINATE EXPANSION IN THE PROPER DIRECTION TO KEEP THE BOTTOMS OF THE BARS NEAR TO THE AXIS LINE.
  # if(geom_name %in% c('bar')) {
  #   plot = plot + scale_y_continuous(expand = expansion(mult = c(0, 0)))
  # }

  #ONLY APPLY WARNINGS RELATED CLASS IF USER WANTS WARNINGS ON.
  if(silence_warnings == FALSE) {
  class(plot) = c("geom_plus_warnings", class(plot))
  }
  plot

}

#' Build a ggplot With the Class "geom_plus_warnings".
#'
#' This method defines how objects of class `geom_plus_warnings`, created by the `ggplot_add.geom_plus()` function, are built into a ggplot2 plot.
#' The method is where various checks are performed to see if the user may be doing something "suboptimal" design-wise that could be used to trigger an informative warning to steer better behaviors.
#'
#' @param plot A ggplot object for which the checks should be performed.
#'
#' @return A built ggplot.
#' @export
ggplot_build.geom_plus_warnings = function(plot) {

    class(plot) = setdiff(class(plot), "geom_plus_warnings") #REMOVE THE TRIGGERING CLASS TO PREVENT RECURSION.

    built = ggplot2:::ggplot_build.ggplot(plot) #BYPASSES MY METHOD SO IT DOESN'T JUST CALL RECURSIVELY, WHICH IT WOULD BECAUSE THE CLASS CHANGE ABOVE WOULDN'T HAVE TAKEN EFFECT YET.

    all_layers = built$plot$layers

  #WARNING CHECKS #1--SEE IF USERS ARE SPECIFYING COLOR/FILL AS A MAPPING. IF SO, SEE IF IT'S FOR A DISCRETE VARIABLE. IF SO, SEE HOW MANY LEVELS ARE BEING GENERATED. IF IT'S MORE THAN X, TRIGGER A WARNING ABOUT POTENTIAL LOSS OF CONTRAST.
  #FIRST, CHECK TO SEE IF FILL AND/OR COLOR ARE MAPPED IN ANY OF THE PLOT'S LAYERS.
  if(any(sapply(all_layers, function(x) { any(c("fill", "colour") %in% names(x$mapping))}))) {

    #IF YES, GO LAYER BY LAYER...
    for(l in all_layers) {
      #IF COLOR AND/OR FILL ARE IN THIS LAYER'S MAPPING...
      if(any(c("fill", "colour") %in% names(l$mapping))) {

        #GRAB THE VARIABLES NAMES OF THE MAPPED VARIABLES (IF ANY)
        if(!is.null(l$mapping$colour)) {
          color_aes = deparse(l$mapping$colour)
          color_var = sub(".*\\(([^()]+)\\).*", "\\1", color_aes) #SANITIZE THE VARIABLE NAME(S) IF AN EXPRESSION HAD BEEN PROVIDED
        } else { color_aes = NULL; color_var = NULL }

        if(!is.null(l$mapping$fill)) {
          fill_aes = deparse(l$mapping$fill)
          fill_var = sub(".*\\(([^()]+)\\).*", "\\1", fill_aes)
          } else { fill_aes = NULL; fill_var = NULL }

        #IF EITHER OF THESE NAMES IS NOT NULL...
        if(!is.null(color_aes) | !is.null(fill_aes)) {

          #IF A USER TRANSFORMS A NUMERIC VARIABLE TO A FACTOR IN THE GGPLOT CALL, ala factor(x), IT'S NOT THE RAW DATA BUT THE COMPUTED DATA WE WANT TO CHECK FOR NUMERIC. PLUS, THE DATA COULD BE IN TWO PLACES...
          if(!is.null(color_aes)) {
          color_dat2check = if(!inherits(l$data, "waiver")) { l$data[[color_var]] } else { built$plot$data[[color_var]] }
          }
          if(!is.null(fill_aes)) {
          fill_dat2check = if(!inherits(l$data, "waiver")) { l$data[[fill_var]] } else { built$plot$data[[fill_var]] }
          }

          #PERFORM THE COLOR CHECK, ASSUMING COLOR WAS MAPPED AND THE VARIABLE WAS DISCRETE.
          if(!is.null(color_aes) &&
              (
                (!is.null(color_dat2check) &&
                !is.numeric(color_dat2check)) |
               (grepl("factor\\(", color_aes)) #IF USERS USED as.factor() OR factor(), THEN WE KNOW WE'RE DEALING WITH DISCRETE VARIABLES NO MATTER WHAT ELSE.
               )
              ) {

            n_levels = length(unique(plot$data[[color_var]])) #GET NUM LEVELS
            #FLAG IF THE NUM LEVELS IS > X
            if(n_levels > 8) {
              warning_text = sprintf("%s, the variable you mapped to color, has %d unique levels. Even with a high-quality palette, it's difficult to ensure all colors chosen will be clearly distinguishable when the number of categories exceeds ~8. In such cases, consider using a different visual channel, intersecting another visual channel (e.g., shape) with color, or reducing the number of groups shown. Set silence_warnings inside geom_plus() to TRUE to hide this and other messages.", color_var, n_levels)

              warning(warning_text, call. = FALSE)
            }
          }

          #SAME FOR FILL AESTHETIC.
          if(!is.null(fill_aes) &&
             ((!is.null(fill_dat2check) &&
              !is.numeric(fill_dat2check)) |
             (grepl("factor\\(", fill_aes)))) {

            n_levels = length(unique(plot$data[[fill_var]]))
            if(n_levels > 8) {
              warning_text = sprintf("%s, the variable you mapped to fill, has %d unique levels. Even with a high-quality palette, it's difficult to ensure all colors chosen will be clearly distinguishable when the number of categories exceeds ~8. In such cases, consider using a different visual channel, intersecting another visual channel (e.g., shape) with color, or reducing the number of groups shown. Set silence_warnings inside geom_plus() to TRUE to hide this and other messages.", fill_var, n_levels)

              warning(warning_text, call. = FALSE)
            }
          }


        }
      }
    }
  }

    #WARNING CHECKS #2--SEE IF USERS HAVE FAILED TO SPECIFY A CUSTOM TITLE FOR EACH SCALE THEY'VE MAPPED. IF THEY HAVEN'T PROVIDE A FRIENDLY WARNING THAT THEY SHOULD CONSIDER DOING SO.

    bad_scales = c() #STORAGE OBJS.
    bad_vars = c()

    #GO THRU ALL LAYERS IN THE PLOT:
    for(l in 1:length(built$plot$layers)) {
    #GO THRU ALL THE MAPPINGS IN THE CURRENT LAYER
    for(m in 1:length(built$plot$layers[[l]]$mapping)) {

      curr_aes = built$plot$layers[[l]]$mapping[m]

      aes_name = names(curr_aes) #WHICH AESTHETIC ARE WE ON?

      #WHAT VARIABLE WAS THAT MAPPED TO? (MUST CLEAN OF EXPRESSION GUNK)
      var_dirty = rlang::as_label(curr_aes[[1]]) #GET THE NAME OF WHATEVER VARIABLE IS MAPPED TO THIS AESTHETIC.
      default_var = sub(".*\\(([^()]+)\\).*", "\\1", var_dirty)

      #USERS CAN SPECIFY CUSTOM NAMES 3 WAYS--VIA LABS(), VIA *LAB(), AND VIA SCALE_*(). THIS CHECKS FOR THE FIRST TWO. ALSO NEEDS CLEANING.
      label_from_labs = built$plot$labels[[aes_name]]
      if (length(label_from_labs) > 0) {
        label_clean = sub(".*\\(([^()]+)\\).*", "\\1", label_from_labs)
      } else {
        label_clean = NULL #WILL NULL OUT IF THERE'S NOTHING HERE.
      }

      #IF THE USER USED A SCALE FUNCTION INSTEAD, LABEL_CLEAN WILL HAVE FOUND JUST THE DEFAULT VARIABLE NAME HERE, SO WE NULL IT BACK OUT TO CONTINUE.
      if(label_clean == default_var) { label_clean = NULL }

      #NOW, WE CHECK TO SEE IF THEY'VE SPECIFIED IT VIA SCALES.
      if(is.null(label_clean)) {
        scale_obj = built$plot$scales$get_scales(aes_name)
        if (!is.null(scale_obj$name) &&
                          !inherits(scale_obj$name, "waiver")) {
         label_clean = sub(".*\\(([^()]+)\\).*", "\\1", scale_obj$name)
        } else {
         label_clean = NULL #WILL AGAIN NULL OUT IF WE FAIL TO FIND ONE.
        }
      }

      #IF THERE IS NO USER PROVIDED NAME, OR THE CLEANED SCALE NAME MATCHES THE CLEANED COLUMN NAME OF THE ORIGINAL DATA, STORE THIS AS A VIOLATION.
      if(is.null(label_clean) || label_clean == default_var) {
        bad_scales = unique(c(bad_scales, aes_name)) #WRAP IN UNIQUE TO PREVENT CATCHING THIS MULTIPLE TIMES.
        bad_vars = unique(c(bad_vars, default_var))
      }
     }
    }

    #TRIGGER THE WARNING IF ANY SCALES VIOLATE.
    if(length(bad_scales) > 0) {
      bad_vars = paste0(bad_vars, collapse = ", ")
      bad_scales = paste0(bad_scales, collapse = ", ")
      warning_text = sprintf("It looks like you haven't provided a custom title for variable(s) %s, mapped to the following aesthetic(s), respectively: %s. This means the title(s) are probably still the column name(s) from your data set, which may not be human-readable, nicely formatted, and intuitive and contain units (if any). We recommend using a scale*() function to specify a custom title for each such scale. Set silence_warnings inside geom_plus() to TRUE to hide this and other messages.", bad_vars, bad_scales)

      warning(warning_text,
              call. = FALSE)

    }

    return(built)
}
