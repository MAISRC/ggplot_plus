#ENHANCED VERSIONS OF EACH COMMON GEOM SO AS TO HAVE IMPROVED DEFAULTS (THAT ARE STILL OVERRIDABLE)

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
                 outlier.fill = "transparent", 
                 alpha = 1),
  violin = list(linewidth = 1.2,
                linetype = "solid",
                colour = "black",
                fill = "white"),
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
  line = list(linewidth = 1.2,
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
                 linetype = "solid")
)


#' Generates Base Geoms With Elevated Defaults
#'
#' Maps inputs to a base ggplot geom (e.g., geom_point or geom_line) but provides default values more likely to adhere to best practices around usability, design aesthetics, and accessibility. 
#'
#' @param geom The name of the geom being drawn. Corresponds to the portion of the geom_ function after the _, e.g., "point" for geom_point, "line" for geom_line, etc. Must be a length one character string, and must match an implemented geom. See names(geom_plus_defaults) for a list of these. Required input.
#' @param ... Other arguments to be passed along to the geom_ function being called.
#' @return List with the class "geom_plus", which will trigger the geom_plus method in ggplot_add. 
#' @examples
#' ggplot(iris, aes(x=Sepal.Length, y=Petal.Length)) + geom_plus(geom = "point")
#' @export
geom_plus = function(geom, ...) {
  if(is.null(geom)) { stop("A geom must be specified!") } #MUST SPECIFY A GEOM
  if(!geom %in% names(geom_plus_defaults)) { stop("The geom you've specified either doesn't exist or hasn't been implemented yet. Please double-check your code for typos. Note that this function only needs the part following the _ in the geom's name.") }
  
  structure(
    list(geom = geom,
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
    plot + layer
  } else {
    plot + layer + scale_shape_manual(values = shape_pal)
  }

}