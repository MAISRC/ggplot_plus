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
#' @param x_title,y_title,color_title,size_title,shape_title,alpha_title A string to use for the graph's legend title for the corresponding aesthetic. Defaults to `NULL` and will be ignored unless a length-1 string. Internally, this passes the new title to `ggplot2::labs()` to circumvent around calling a `scale_*` function, which might overwrite intended behaviors.
#' @param silence_warnings `geom_plus()` triggers some checks for aspects of good graph design and, if any of these checks fail, a warning is triggered to direct the user towards better practices. Set this parameter to `FALSE` to silence these warnings. To set TRUE as the default for the current session, run `options(geom_plus_silence_warnings = TRUE)`.
#'
#' @details
#' If you map the **shape** aesthetic, `geom_plus()` automatically adds
#' `ggplot2::scale_shape_manual()` with the palette of shapes = 21–25.
#'
#' If you later add your own `scale_shape_*()` layer, it will *replace*
#' the default palette with a warning.
#'
#' To change only the legend title while keeping the default palette,
#' use the `shape_title` argument or `ggplot2::labs()` instead of adding a new scale.
#'
#' A similar pattern occurs with fill and color, as these are governed by
#' `scale_fill_viridis_*()` or `scale_color_viridis_*()`, respectively.
#' See `?palettes_plus()` for details.
#'
#' @return List with the class "geom_plus", which will trigger the geom_plus method in ggplot_add.
#' @examples
#' #BASIC USAGE
#' ggplot2::ggplot(iris, ggplot2::aes(x=Sepal.Length, y=Petal.Length)) +
#' geom_plus(geom = "point")
#' @export
geom_plus = function(geom,
                     ...,
                    include_theme = FALSE,
                    include_gridlines = FALSE,
                    include_xscale_plus = FALSE,
                    include_yscale_plus = FALSE,
                    include_fillscale_plus = FALSE,
                    include_colorscale_plus = FALSE,
                    x_title = NULL,
                    y_title = NULL,
                    color_title = NULL,
                    fill_title = NULL,
                    size_title = NULL,
                    shape_title = NULL,
                    alpha_title = NULL,
                    silence_warnings = getOption("geom_plus_silence_warnings", FALSE)) {
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
         x_title = x_title,
         y_title = y_title,
         fill_title = fill_title,
         color_title = color_title,
         size_title = size_title,
         shape_title = shape_title,
         alpha_title = alpha_title,
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
#' @param object_name Internal name used by ggplot2 when adding the layer.
#'
#' @return A ggplot object with the new geometry layer added.
#' @export
ggplot_add.geom_plus = function(object, plot, object_name) {

  user_args = object$user_args #LOCALLY PROVIDED USER ARGUMENTS.
  geom_name = object$geom #UNPACK THE GEOM CHOSEN
  silence_warnings = object$silence_warnings #UNPACK USER DESIRES FOR WARNINGS.
  include_theme = object$include_theme #UNPACK USER DESIRES FOR THEME.
  include_gridlines = object$include_gridlines #UNPACK USER DESIRES AROUND GRIDLINES.
  include_xscale_plus = object$include_xscale_plus #UNPACK USER DESIRES AROUND X SCALE ADJUSTMENT.
  include_yscale_plus = object$include_yscale_plus #UNPACK USER DESIRES AROUND Y SCALE ADJUSTMENT.
  include_fillscale_plus = object$include_fillscale_plus #UNPACK USER DESIRES AROUND FILL SCALE ADJUSTMENT.
  include_colorscale_plus = object$include_colorscale_plus #UNPACK USER DESIRES AROUND COLOR SCALE ADJUSTMENT.
  x_title = object$x_title
  y_title = object$y_title
  fill_title = object$fill_title
  color_title = object$color_title
  size_title = object$size_title
  shape_title = object$shape_title
  alpha_title = object$alpha_title

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
  pmatch_idx = pmatch("inherit.aes", names(user_args), nomatch = 0L)
  if (pmatch_idx > 0L) { user_args[[pmatch_idx]] } else { user_args$inherit.aes = TRUE }

  #SO, THINGS GO SQUIRRELY WHEN USERS TRY TO SET CONSTANTS INSIDE OF AES() INSTEAD OF OUTSIDE OF IT. WE WILL GO THRU ALL THE MAPPED AESTHETICS AND, IF ANY ARE MAPPED TO CONSTANTS, WE WEED THOSE OUT AND PUT THEM IN USER_ARGS WHERE THEY BELONGED.
  result_local = extract_mapped_constants(aes_local, user_args, silence = silence_warnings, location = "geom")
  aes_local = result_local$aes
  user_args = result_local$constants

  #ONLY DO IF INHERIT == T BECAUSE OTHERWISE ONLY LOCAL AES MATTERS AT THIS MOMENT.
  if(user_args$inherit.aes) {
  result_global = extract_mapped_constants(aes_global, user_args, silence = silence_warnings, location = "ggplot")
  aes_global = result_global$aes
  user_args = result_global$constants
  }

  if(user_args$inherit.aes) {
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

  geom_str = paste0("geom_", geom_name)

  #THE IF/ELSE LOGIC HERE ALLOWS US TO NAMESPACE GGPLOT2 SAFELY BUT AVOID DOING SO WITH MY CUSTOM GEOMS.
  if(geom_str == "geom_point_plus") {
    layer = do.call(geom_point_plus, use_these_args)
  } else {
    geom_fn = getExportedValue("ggplot2", geom_str) #GET THE ACTUAL FUNCTION THAT THE USER IS HOPING WE'LL USE HERE.
    layer = do.call(geom_fn, use_these_args)
  }


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
  if(exists("x_title") &&
     is.character(x_title) &&
     length(x_title) == 1) {
    plot = plot + ggplot2::xlab(x_title)
  }
  if(exists("y_title") &&
     is.character(y_title) &&
     length(y_title) == 1) {
    plot = plot + ggplot2::ylab(y_title)
  }
  if(exists("fill_title") &&
     is.character(fill_title) &&
     length(fill_title) == 1) {
    plot = plot + ggplot2::labs(fill = fill_title)
  }
  if(exists("color_title") &&
     is.character(color_title) &&
     length(color_title) == 1) {
    plot = plot + ggplot2::labs(colour = color_title)
  }
  if(exists("size_title") &&
     is.character(size_title) &&
     length(size_title) == 1) {
    plot = plot + ggplot2::labs(size = size_title)
  }
  if(exists("shape_title") &&
     is.character(shape_title) &&
     length(shape_title) == 1) {
    plot = plot + ggplot2::labs(shape = shape_title)
  }
  if(exists("alpha_title") &&
     is.character(alpha_title) &&
     length(alpha_title) == 1) {
    plot = plot + ggplot2::labs(alpha = alpha_title)
  }

  class(plot) = c("geom_plus_warnings", class(plot))
  plot$silence_warnings = silence_warnings #CARRY THIS THRU ON PLOT
  plot$custom_shapes = ifelse(geom_name == "point_plus", TRUE, FALSE) #ALSO CARRY THRU A CHECK OF WHETHER WE'RE USING POINT_PLUS
  plot

}

#' Build a ggplot With the Class "geom_plus_warnings".
#'
#' This method defines how objects of class `geom_plus_warnings`, created by the `ggplot_add.geom_plus()` function, are built into a ggplot2 plot.
#' The method is where various checks are performed to see if the user may be doing something "sub-optimal" design-wise that could be used to trigger an informative warning to steer better behaviors.
#'
#' @param plot A ggplot object for which the checks should be performed.
#'
#' @return A built ggplot.
#' @export
ggplot_build.geom_plus_warnings = function(plot) {

    class(plot) = setdiff(class(plot), "geom_plus_warnings") #REMOVE THE TRIGGERING CLASS TO PREVENT RECURSION.

    built = ggbuild_ggplot(plot) #THEN BUILD THE PLOT. BYPASSES MY METHOD SO IT DOESN'T JUST CALL RECURSIVELY, WHICH IT WOULD BECAUSE THE CLASS CHANGE ABOVE WOULDN'T HAVE TAKEN EFFECT YET.

    #HERE, WE PAUSE TO ASK--IS ALPHA MAPPED TO A CONSTANT < 1? IS THERE A POTENTIAL LEGEND? IF SO, WE SHOULD OVERRIDE THE LEGEND'S ALPHA TO BE 1 FOR EASIER READING.
    global_aes = names(built$plot$mapping) #GLOBAL MAPPINGS
    layer_aes = unlist(lapply(built$plot$layers, function(l) names(l$mapping))) #LOCAL MAPPINGS
    present_aes = unique(c(global_aes, layer_aes)) #ALL MAPPINGS

    aesthetics_to_override = intersect(present_aes,
                                       c("colour", "fill", "shape", "linetype", "size")) #THE MAPPINGS MOST LIKELY TO HAVE LEGENDS OF CONCERN HERE.

    #FOR EACH POTENTIAL GUIDE, BUILD A LIST OF AESTHETICS OVERRIDES, INCLUDING AN ALPHA OF 1 AND, SO LONG AS IT'S NOT A SIZE LEGEND, SIZE.
    guide_overrides = lapply(aesthetics_to_override, function(aes) {
      #THESE ARE THE OVERRIDE CONDITIONS...
      override = list(alpha = 1)
      if (aes != "size") {
        override$size = ifelse(plot$custom_shapes == TRUE, 10, 5) #MAKES THE KEY TWICE AS BIG FOR THE CUSTOM SHAPES AS FOR GGPLOT'S REGULAR SHAPES.
      }

      #HOWEVER, WE ONLY WANT TO USE THEM IF THE LEGEND IS NOT A COLOR BAR, AS SIZE AND ALPHA ARE NOT RELEVANT FOR THOSE.
      scale_obj = built$plot$scales$get_scales(aes)
      is_continuous = inherits(scale_obj, "ScaleContinuous")

      if (aes %in% c("fill", "colour", "color") && is_continuous) {
        ggplot2::guide_colourbar()
      } else {
        ggplot2::guide_legend(override.aes = override)
      }
    })
    names(guide_overrides) = aesthetics_to_override

    for(layer_data in built$data) { #GO LAYER BY LAYER.
      if(any(c("alpha", "size") %in% names(layer_data))) { #IF THIS LAYER'S DATA CONTAINS AN ALPHA OR SIZE COLUMN...
        unique_alpha = unique(layer_data$alpha) #DETERMINE IF IT'S A CONSTANT.
        unique_size = unique(layer_data$size)

        if((length(unique_alpha) == 1 &&
            !is.na(unique_alpha) &&
            is.numeric(unique_alpha) &&
            unique_alpha < 1 ) ||
           (length(unique_size) == 1 && unique_size < 5)) { #IF IT IS AND IT'S LESS THAN 1 OR THE DEFAULT SIZE...

          #ATTEMPT TO OVERRIDE THE ALPHA AND SIZE VALUES OF JUST THE LEGEND (JUST ONCE!).
          if(is.null(built$legend_keys_overridden)) {
          rebuild = built$plot +
            do.call(ggplot2::guides, guide_overrides)
          built = suppressWarnings(ggbuild_ggplot(rebuild)) #REBUILD FROM HERE. #FOR CERTAIN COMPATIBLE AESTHETICS, E.G., SHAPE AND COLOUR, WE'LL BE APPLYING TWO GUIDE OVERRIDES TO ONE, UNIFIED LEGEND, WHICH IS REDUNDANT BUT NOT HARMFUL, SO WE JUST SUPPRESS THE WARNING WE'D GET IF WE TRIED.
          built$legend_keys_overridden = TRUE #A FLAG TO KEEP THIS FROM TRIGGERING MULTIPLE TIMES
          break #NO NEED TO DO THIS MORE THAN ONCE PER LAYER...
          }
        }
       }
      }

    all_layers = built$plot$layers

    if(plot$silence_warnings == FALSE) {

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

      ###DEPRECATED--THE SCALE_*_CONTINUOUS_PLUS() FUNCTIONS ALREADY CHECK THIS INTERNALLY AND WARN ABOUT IT.
    # #WARNING CHECKS #2--SEE IF USERS HAVE FAILED TO SPECIFY A CUSTOM TITLE FOR EACH SCALE THEY'VE MAPPED. IF THEY HAVEN'T PROVIDE A FRIENDLY WARNING THAT THEY SHOULD CONSIDER DOING SO.
    #
    # bad_scales = c() #STORAGE OBJS.
    # bad_vars = c()
    #
    # #GO THRU ALL LAYERS IN THE PLOT:
    # for(l in 1:length(built$plot$layers)) {
    # #GO THRU ALL THE MAPPINGS IN THE CURRENT LAYER
    # for(m in 1:length(built$plot$layers[[l]]$mapping)) {
    #
    #   curr_aes = built$plot$layers[[l]]$mapping[m]
    #
    #   aes_name = names(curr_aes) #WHICH AESTHETIC ARE WE ON?
    #
    #   #WHAT VARIABLE WAS THAT MAPPED TO? (MUST CLEAN OF EXPRESSION GUNK)
    #   var_dirty = rlang::as_label(curr_aes[[1]]) #GET THE NAME OF WHATEVER VARIABLE IS MAPPED TO THIS AESTHETIC.
    #   default_var = sub(".*\\(([^()]+)\\).*", "\\1", var_dirty)
    #
    #   #USERS CAN SPECIFY CUSTOM NAMES 3 WAYS--VIA LABS(), VIA *LAB(), AND VIA SCALE_*(). THIS CHECKS FOR THE FIRST TWO. ALSO NEEDS CLEANING.
    #   label_from_labs = built$plot$labels[[aes_name]]
    #   if (length(label_from_labs) > 0) {
    #     label_clean = sub(".*\\(([^()]+)\\).*", "\\1", label_from_labs)
    #   } else {
    #     label_clean = NULL #WILL NULL OUT IF THERE'S NOTHING HERE.
    #   }
    #
    #   #IF THE USER USED A SCALE FUNCTION INSTEAD, LABEL_CLEAN WILL HAVE FOUND JUST THE DEFAULT VARIABLE NAME HERE, SO WE NULL IT BACK OUT TO CONTINUE.
    #   if(label_clean == default_var) { label_clean = NULL }
    #
    #   #NOW, WE CHECK TO SEE IF THEY'VE SPECIFIED IT VIA SCALES.
    #   if(is.null(label_clean)) {
    #     scale_obj = built$plot$scales$get_scales(aes_name)
    #     if (!is.null(scale_obj$name) &&
    #                       !inherits(scale_obj$name, "waiver")) {
    #      label_clean = sub(".*\\(([^()]+)\\).*", "\\1", scale_obj$name)
    #     } else {
    #      label_clean = NULL #WILL AGAIN NULL OUT IF WE FAIL TO FIND ONE.
    #     }
    #   }
    #
    #   #IF THERE IS NO USER PROVIDED NAME, OR THE CLEANED SCALE NAME MATCHES THE CLEANED COLUMN NAME OF THE ORIGINAL DATA, STORE THIS AS A VIOLATION.
    #   if(is.null(label_clean) || label_clean == default_var) {
    #     bad_scales = unique(c(bad_scales, aes_name)) #WRAP IN UNIQUE TO PREVENT CATCHING THIS MULTIPLE TIMES.
    #     bad_vars = unique(c(bad_vars, default_var))
    #   }
    #  }
    # }
    #
    # #TRIGGER THE WARNING IF ANY SCALES VIOLATE.
    # if(length(bad_scales) > 0) {
    #   bad_vars = paste0(bad_vars, collapse = ", ")
    #   bad_scales = paste0(bad_scales, collapse = ", ")
    #   warning_text = sprintf("It looks like you haven't provided a custom title for variable(s) %s, mapped to the following aesthetic(s), respectively: %s. This means the title(s) are probably still the column name(s) from your data set, which may not be human-readable, nicely formatted, and intuitive and contain units (if any). We recommend using a scale*() function to specify a custom title for each such scale. Set silence_warnings inside geom_plus() to TRUE to hide this and other messages.", bad_vars, bad_scales)
    #
    #   warning(warning_text,
    #           call. = FALSE)
    #
    # }

      ###DE-ACTIVATED FOR NOW--IT BREAKS WITH FILL BECAUSE IT WILL MAP FILLS TO HEXCODES WHICH LOOK LIKE CHARACTERS IN THIS CONTEXT...###
  #   #WARNING CHECKS #3--SEE IF USERS HAVE CONTINUOUS SCALES THAT ARE LACKING LABELS AT OR NEAR THEIR LIMITS. THEY SHOULD CONSIDER USING OUR SCALE_*_CONTINUOUS_PLUS FUNCTIONS FOR THESE SCALES.
  #
  #   #THIS IS A CONVENIENCE FUNCTION (THANKS CHATGPT) THAT TAKES A GGPLOT OBJECT AND AN AESTHETIC, LIKE "x", AND FIGURES OUT IF IT'S MAPPED AND, IF SO, FIGURES OUT WHAT VARIABLE IT WAS MAPPED TO.
  #   find_aes_source = function(gg, aes) {
  #
  #     plot = if (inherits(gg, "ggplot")) gg else gg$plot #FIGURE OUT WHERE TO LOOK
  #
  #     #SCAN THROUGH THE LAYERS TO SEARCH THERE (THESE WOULD WIN IF THERE IS ONE)
  #     for (L in rev(plot$layers)) {
  #       if (!is.null(L$mapping[[aes]])) {
  #         return(rlang::as_label(L$mapping[[aes]]))
  #       }
  #       if (!is.null(L$aes_params[[aes]])) {
  #         return(NA)
  #       }
  #     }
  #
  #     #OTHERWISE GET IT FROM GLOBAL IF IT'S THERE.
  #     if (!is.null(plot$mapping[[aes]])) {
  #       return(rlang::as_label(plot$mapping[[aes]]))
  #     }
  #
  #     #OTHERWISE JUST FAIL--MUST NOT BE MAPPED.
  #     return(NA)
  #   }
  #
  #   #ANOTHER CONVENIENCE FUNCTION FROM CHATGPT THAT TAKES A BUILT GGPLOT OBJECT AND AN AESTHETIC AND FINDS THE RANGE OF THE VARIABLE MAPPED TO THAT AESTHETIC (ASSUMING IT'S CONTINUOUS)
  #   range_from_built_data = function(built, aes) {
  #     vals = unlist(lapply(seq_along(built$data), function(i) {
  #       df = built$data[[i]]
  #       message("Layer ", i, " | cols: ", paste(names(df), collapse = ", "))
  #
  #       # Direct column check
  #       if (aes %in% names(df)) {
  #         message("  Found raw aes: ", aes, "; class: ", class(df[[aes]]))
  #         if (is.numeric(df[[aes]])) {
  #           return(df[[aes]])
  #         } else {
  #           message("  Skipping: not numeric.")
  #         }
  #       }
  #
  #       # Special fallback for x/y
  #       fallback_cols = switch(
  #         aes,
  #         x = intersect(c("xmin_final", "xmax_final"), names(df)),
  #         y = intersect(c("ymin_final", "ymax_final"), names(df)),
  #         character(0)
  #       )
  #
  #       if (length(fallback_cols)) {
  #         message("  Using fallback columns: ", paste(fallback_cols, collapse = ", "))
  #         return(unlist(df[, fallback_cols, drop = FALSE]))
  #       }
  #
  #       return(NULL)
  #     }))
  #
  #     vals = vals[is.finite(vals)]
  #     if (length(vals)) range(vals) else NULL
  #   }
  #
  #   #NOW, WE CYCLE THRU EACH OF THE RELEVANT AESTHETICS
  #   scales2warn = c()
  #   for(sc in c("x", "y", "colour", "fill")) {
  #     scale.mapped = find_aes_source(built, sc) #CHECK TO SEE IF THIS SCALE WAS EVEN MAPPED TO A VARIABLE.
  #     if(!is.null(scale.mapped) &&
  #        !is.na(scale.mapped)) {
  #
  #       scale.attr = built$plot$scales$get_scales(sc) #IF SO, FIND THAT SCALE IN THE PLOT'S SCALES.
  #
  #       print(class(scale.attr))
  #       if(inherits(scale.attr, "ScaleContinuous")) { #FIND OUT IF IT'S A CONTINUOUS SCALE
  #
  #         #PERFORM THE SAME CHECK AS IN THE SCALE_*_CONTINUOUS_PLUS FUCTIONS.
  #         scale.range = range_from_built_data(built, sc) #RANGE OF ACTUAL DATA
  #         lower = scale.range[1]
  #         upper = scale.range[2]
  #         span = upper-lower
  #
  #         brks = built$layout$panel_params[[1]][[sc]]$breaks #THE ASSIGNED BREAKPOINTS
  #         brks = brks[!is.na(brks)] #SCRUB OUT NAS
  #         buffer_frac = 0.05
  #         buffer = buffer_frac * span
  #
  #         got_low  = min(brks) <= (lower + buffer)
  #         got_high = max(brks) >= (upper - buffer)
  #
  #         #IF THE BREAKS SEEM SUBOPTIMAL, WE FLAG THAT WITH A WARNING.
  #         if(!got_low || !got_high) {
  #           scales2warn = c(scales2warn, sc)
  #         }
  #
  # # ##WARNING #4: WHILE WE'RE CHECKING CONTINUOUS AXES, WE CAN CHECK THE Y AXIS TO SEE IF 0 IS WITHIN THE LIMITS. IF IT ISN'T, WARNING COULD REMIND USERS THAT 0 IS A COMMON NULL HYPOTHESIS VALUE AND THAT IT MIGHT BE APPROPRIATE TO INCLUDE IT.
  # #         if(sc == "y" &&
  # #            !is_between(low = lower, high = upper, value = 0)) {
  # #
  # #           warning("It appears that y is mapped to a continuous variable, but 0 is not within the y-axis limits. 0 is a common null hypothesis value; consider whether it would be appropriate to expand the axis limits to include it. Set silence_warnings inside geom_plus() to TRUE to hide this and other messages.")
  # #
  # #         } #DEPRECATED WHILE I CONSIDER HOW BEST TO IMPLEMENT.
  #       }
  #
  #     }
  #
  #   }
  #   if(length(scales2warn) > 0) {
  #   warning(paste0("Your [", paste0(scales2warn, collapse = ", "), "] scale(s) seem(s) to be lacking a break near the upper and/or lower limit(s) of the data. Consider using the corresponding scale_*_continuous_plus() function(s) to address this issue. Set silence_warnings inside geom_plus() to TRUE to hide this and other messages."))
  #    }
   }

    class(built) = c("geom_plus_warnings_built", class(built))

    return(built)
}
