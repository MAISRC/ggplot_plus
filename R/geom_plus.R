#' Generates Base Geoms With Elevated Defaults
#'
#' Maps inputs to a base ggplot geom (e.g., geom_point or geom_line) but provides default values more likely to adhere to best practices around usability, design aesthetics, and accessibility.
#'
#' @param geom The name of the geom being drawn. Corresponds to the portion of the geom_ function after the _, e.g., "point" for geom_point, "line" for geom_line, etc. Must be a length one character string, and must match an implemented geom. See names(geom_plus_defaults) for a list of these. Required input.
#' @param ... Other arguments to be passed along to the geom_ function being called.
#' @param include_theme Should a call to `theme_plus()` with no arguments be automatically applied to the ggplot command chain, without needing to be called separately? Defaults to `FALSE`. Set to `TRUE` to include it.
#' @param include_gridlines Should a call to `gridlines_plus()` with no arguments be automatically applied to the ggplot command chain, without needing to be called separately? Defaults to `FALSE`. Set to `TRUE` to include it.
#' @param include_ytitle_plus Should a call to `yaxis_title_plus()` with no arguments be automatically applied to the ggplot command chain, without needing to be called separately? Defaults to `FALSE`. Set to `TRUE` to include it.
#' @param include_xscale_plus Should a call to `scale_x_continuous_plus()` with no arguments be automatically applied to the ggplot command chain, without needing to be called separately? Defaults to `FALSE`. Set to `TRUE` to include it.
#' @param include_yscale_plus Should a call to `scale_y_continuous_plus()` with no arguments be automatically applied to the ggplot command chain, without needing to be called separately? Defaults to `FALSE`. Set to `TRUE` to include it.
#' @param include_fillscale_plus Should a call to `scale_fill_continuous_plus()` with no arguments be automatically applied to the ggplot command chain, without needing to be called separately? Defaults to `FALSE`. Set to `TRUE` to include it.
#' @param include_colorscale_plus Should a call to `scale_color_continuous_plus()` with no arguments be automatically applied to the ggplot command chain, without needing to be called separately? Defaults to `FALSE`. Set to `TRUE` to include it.
#' @param x_title,y_title,color_title,size_title,shape_title,alpha_title,fill_title A string to use for the graph's legend title for the corresponding aesthetic. Defaults to `NULL` and will be ignored unless a length-1 string. Internally, this passes the new title to `ggplot2::labs()` to circumvent around calling a `scale_*` function, which might overwrite intended behaviors.
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
                    include_ytitle_plus = FALSE,
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
         include_ytitle_plus = include_ytitle_plus,
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
  include_ytitle_plus = object$include_ytitle_plus #UNPACK USER DESIRES AROUND MOVING THE Y AXIS TITLE.
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
  if(include_ytitle_plus == TRUE) {
    plot = plot + yaxis_title_plus() #INCLUDE A VANILLA yaxis_title_plus() CALL IF THIS IS TRUE.
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


  plot = .ensure_ggplot_plus(plot) #MAKE SURE WE HAND OFF TO COMMON BUILD METHOD AND RECORD INTENT/MANAGE CLASSES/GENERATE STORAGE.

  #FLAG GUIDE-RELATED UPDATES NEEDED IN BUILD.
  plot$ggplot_plus$guides = utils::modifyList(
    plot$ggplot_plus$guides %||% list(),
    list(needs_override = TRUE,
         custom_shapes = isTRUE(geom_name %in% c("point_plus")) )
  )

  #FLAG WARNINGS RELATED UPDATES NEEDED IN BUILD.
  plot$ggplot_plus$warnings = modifyList(
    plot$ggplot_plus$warnings %||% list(),
    list(silence = isTRUE(silence_warnings))
  )

  plot

}
