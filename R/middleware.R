#CREATE A LITTLE ENVIRONMENT FOR PALETTE KEYS SO WE CAN MANAGE/RESTORE THEM CLEANLY.
.ggplotplus_palette_env = new.env(parent = emptyenv())

#PLOP THESE PALETTE KEYS IN THERE.
.ggplotplus_palette_keys = c(
  "ggplot2.discrete.fill", "ggplot2.discrete.colour",
  "ggplot2.continuous.fill", "ggplot2.continuous.colour",
  "ggplot2.binned.fill", "ggplot2.binned.colour"
)

#' Default settings for geometry layers created by `geom_plus()`
#'
#' A named list of default aesthetics used by `geom_plus()` to control styling of the resulting geometry layers.
#'
#' @format A named list with elements like "point", "jitter", "boxplot", etc., corresponding to commonly used ggplot2 geometries. Use names(geom_plus_defaults) for a full list.
#' @export
geom_plus_defaults = list(
  point = list(size = 5,
               shape = 21,
               stroke = 1.2,
               fill = "transparent",
               colour = "black"),
  point_plus = list(colour = "black",
                    fill = NA,
                    size = 4,
                    alpha = 1,
                    stroke = 1.1),
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


#' HELPER FUNCTION THAT WILL SEARCH THRU A BUILT GGPLOT2 OBJECT TO DETERMINE WHETHER A GIVEN POSITIONAL AXIS (X OR Y) IS CONTINUOUS.
#' @param built A built plot (the return value of `ggplot2::ggplot_build()`).
#' @param axis Character; `"x"` or `"y"`.
#'
#' @return Logical scalar. `TRUE` if at least one panel’s axis is continuous,
#'   otherwise `FALSE` (or when the axis does not exist).
#'
#' @details
#' The function prefers `built$layout$panel_params` (available in modern
#' ggplot2), falling back to `panel_scales_x` / `panel_scales_y`, and finally
#' to trained `built$plot$scales`. It treats scale classes inheriting from
#' `"ScaleContinuous"` (and `"ScaleBinned"`) as continuous.
#'
#' @seealso [.aes_is_continuous()], [ggplot_build.ggplot_plus()]
#'
#' @keywords internal
#' @noRd
.axis_is_continuous = function(built, axis = c("x","y")) {

  axis = match.arg(axis)

  ## 1ST PLACE TO LOOK--IN LAYOUT$PANEL_PARAMS (MORE MODERN) AS PANELS MAY HAVE FREE X/YS
  pp = built$layout$panel_params
  if(!is.null(pp) && length(pp) > 0) {
    #GO THRU EACH PANEL'S PARAMETERS
    got = vapply(pp, function(p) {
      ax = tryCatch(p[[axis]], error = function(e) NULL) #IS THAT AXIS A THING?
      if(is.null(ax)) { return(FALSE) }

      # IS THERE A SCALE OBJECT?
      sc = tryCatch(ax$scale, error = function(e) NULL)
      if(!is.null(sc)) {
        #IF SO, FIGURE OUT IF IT INHERITS THE CONTINUOUS CLASSES.
        return(inherits(sc, "ScaleContinuous") || inherits(sc, "ScaleContinuousPosition"))
      }

      # IS THERE A BREAKS OBJECT? THAT IMPLIES CONTINUOUS (INCLUDING DATES)
      br = tryCatch(ax$breaks, error = function(e) NULL)
      if(!is.null(br)) {
        return(is.numeric(br) || inherits(br, "Date") || inherits(br, "POSIXt"))
      }

      # SAME LOGIC WITH A NUMERIC RANGE?
      rng = tryCatch(ax$range, error = function(e) NULL)
      if(!is.null(rng)) {
        return(is.numeric(rng))
      }

      FALSE
    }, logical(1))
    if(any(got)) { return(TRUE) }
  }

  ## OLDER GGPLOT2 OBJECTS WILL HAVE A PANELS_SCALES LIST FOR EACH PANEL.
  sc_list = built$layout[[paste0("panel_scales_", axis)]]
  if(!is.null(sc_list) && length(sc_list) > 0) {
    #GO THRU EACH OF THE PANELS HERE FOR THIS AXIS AND SEE IF THEY INHERIT THE CONTINUOUS SCALES.
    got = vapply(sc_list, function(s) {
      inherits(s, "ScaleContinuous") || inherits(s, "ScaleContinuousPosition")
    }, logical(1))
    if(any(got)) { return(TRUE) }
  }

  ## AS A LAST RESORT, LOOK TO SEE IF THERE ARE TRAINED AXIS SCALES ON THE PLOT ITSELF. IF SO, AGAIN CHECK CLASS.
  sc = built$plot$scales$get_scales(axis)
  if(!is.null(sc)) {
    return(inherits(sc, "ScaleContinuous") || inherits(sc, "ScaleContinuousPosition"))
  }

  FALSE #FAIL.
}

#' SAME IDEA FOR COLOR/FILL AND OTHER NON-POSITIONAL AXES.
#' @param built A built plot (from `ggplot2::ggplot_build()`).
#' @param aes One of `"colour"`, `"fill"`, `"size"`, `"alpha"`, `"linewidth`, `"linetype"`, , `"shape"`.
#'
#' @return Logical scalar. `TRUE` if the aesthetic is continuous or binned;
#'   `FALSE` if it is discrete or could not be located.
#'
#' @details
#' Primary check uses `built$plot$scales$get_scales(aes)` and returns `TRUE`
#' when the scale inherits from `"ScaleContinuous"` or `"ScaleBinned"`.
#' As a fallback, it inspects panel parameters for numeric/date/datetime breaks.
#'
#' @seealso [.axis_is_continuous()], [ggplot_build.ggplot_plus()]
#'
#' @keywords internal
#' @noRd
.aes_is_continuous = function(built, aes = c("colour","fill","size","alpha", "linetype", "shape", "linewidth")) {

  aes = match.arg(aes)

  # LOOKING AT TRAINED SCALES HERE IS BEST, AS NON-POSITIONALS WILL OFTEN HAVE LEGENDS.
  sc = built$plot$scales$get_scales(aes)
  if(!is.null(sc)) {
    #BINNED COUNTS AS CONTINUOUS HERE.
    return(inherits(sc, "ScaleContinuous") || inherits(sc, "ScaleBinned"))
  }

  # FOR NON-POSITIONS, TRY A RARE FALL BACK OF THE PANEL_PARAMETERS AND LOOK FOR THE AES OR BREAKS
  pp = built$layout$panel_params
  if(!is.null(pp) && length(pp) > 0) {
    got = vapply(pp, function(p) {
      ax = tryCatch(p[[aes]], error = function(e) NULL)
      if(is.null(ax)) { return(FALSE) }
      br = tryCatch(ax$breaks, error = function(e) NULL)
      if(!is.null(br)) {
        return(is.numeric(br) || inherits(br, "Date") || inherits(br, "POSIXt"))
      }
      FALSE
    }, logical(1))
    if(any(got)) { return(TRUE) }
  }

  FALSE #FAIL
}

#' THIS FUNCTION IS A HELPER FOR DETERMINING IF A GIVEN SCALE/AES HAS ITS GUIDE TURNED OFF OR NOT. RETURNS FALSE IF A GUIDE IS OFF AND THUS SHOULD NOT BE CONSIDERED FURTHER.
#' @param built A built plot (from `ggplot2::ggplot_build()`).
#' @param aes Aesthetic name, e.g. `"colour"`, `"fill"`, `"size"`, `"alpha"`.
#'
#' @return Logical scalar. `TRUE` if a guide might be shown for this aesthetic;
#'   `FALSE` if it is likely hidden.
#'
#' @details
#' We check the trained scale via `built$plot$scales$get_scales(aes)` and treat
#' `NULL`/missing as “guide allowed” unless we can detect a `"none"`.
#' Because guides can be merged or declared in multiple ways, this takes a
#' conservative stance: it only returns `FALSE` when suppression is explicit.
#'
#' @seealso [ggplot_build.ggplot_plus()]
#'
#' @keywords internal
#' @noRd
.should_consider_guide = function(built, aes) {

  #THEY MIGHT HAVE KILLED ALL GUIDES VIA THEME...
  pos = tryCatch(ggplot2::calc_element("legend.position", built$plot$theme), error = function(e) NULL)
  if(is.character(pos) && identical(pos, "none")) { return(FALSE) }

  # HAS A SPECIFIC AES BEEN SET TO "none" VIA guides()?
  gspec = tryCatch(built$plot$guides, error = function(e) NULL)
  if(!is.null(gspec) && !is.null(gspec$guides)) {
    val = gspec$guides[[aes]]
    if(is.character(val) && identical(val, "none")) { return(FALSE) }
    if(inherits(val, "guide_none")) { return(FALSE) }  #ANOTHER POSSIBILITY
  }

  #WE CAN ALSO CHECK THE PLOT SCALES AND SEE IF THOSE HAVE A GUIDE SLOT THAT IS EQUAL TO NONE.
  sc = built$plot$scales$get_scales(aes)
  if(!is.null(sc) && identical(sc$guide, "none")) { return(FALSE) }

  #LASTLY, USERS CAN USE show.legend TO TURN OFF GUIDES BY LAYER. WE CAN CHECK IF THEY HAVE TURNED OFF A GUIDE FOR AN AES FOR EVERY POSSIBLE LAYER.
  has_candidate_layer = FALSE
  for(l in built$plot$layers) {
    if(isFALSE(l$show.legend)) { next } #ALL LEGENDS OFF FOR THIS LAYER...
    if(!is.null(l$mapping[[aes]])) { has_candidate_layer = TRUE; break } #AH THIS ONE IS ON FOR THIS AES.
  }
  if(!has_candidate_layer) { return(FALSE) }

  TRUE #MUST BE ON!
}


#' THIS LITTLE HELPER WILL PULL THE NUMBER OF LEVELS OUT OF A DISCRETE COLOR/FILL SCALE (IF THAT'S WHAT IT IS):
#' @param built A built plot (from `ggplot2::ggplot_build()`).
#' @param aes Character; `"colour"` or `"fill"`.
#'
#' @return Integer count of levels, or `NA_integer_` if unknown (e.g., unmapped,
#'   continuous scale, or insufficient information).
#'
#' @seealso [ggplot_build.ggplot_plus()], [.aes_is_continuous()]
#'
#' @keywords internal
#' @noRd
.aes_num_levels = function(built,
                           aes = c("colour","fill")) {

  aes = match.arg(aes)

  sc = built$plot$scales$get_scales(aes)

  if(is.null(sc) || !inherits(sc, "ScaleDiscrete")) { return(NA_integer_) } #FAIL EARLY

  #WE HAVE TO LOOK IN A COUPLE OF DIFFERENT PLACES FOR THE NUMBER OF LEVELS. A SLOT, MOSTLY IN OLDER GGPLOT, IS THE RANGEDISCRETE$RANGE SLOT
  rng = tryCatch(sc$range$range, error = function(e) NULL)
  if(!is.null(rng) &&
     length(rng)) {
    rng = rng[!is.na(rng)]
    if(length(rng)) {return(length(unique(rng)))}
  }

  #THE NEXT BEST WOULD BE TO LOOK AT COMPUTED BREAKS IF THEY EXIST YET. THE DOWNSIDE IS THAT THE USER MIGHT MANIPULATE THIS TO SHRINK IT.
  br = tryCatch(sc$get_breaks(), error = function(e) NULL)
  if(!is.null(br) &&
     !inherits(br, "waiver")) {
    br = br[!is.na(br)]
    if(length(br)) { return(length(unique(br))) }
  }

  #A THIRD SPOT IS THE NUMBER OF UNIQUE VALUES MAPPED TO THE AESTHETIC IN BUILT$DATA
  vals = unlist(lapply(built$data, function(df) df[[aes]]), use.names = FALSE)
  vals = vals[!is.na(vals)]
  if(length(vals)) {return(length(unique(vals)))}

  #ANOTHER OPTION WOULD BE TO SEE IF THE NUMBER OF PALETTE SWATCHES PULLED IS AVAILABLE.
  pc = tryCatch(sc$palette.cache, error = function(e) NULL)
  if (!is.null(pc) && length(pc)) {return(length(pc))}

  NA_integer_ #FAIL
}


#' GPAR ARGUMENTS AND THEME ARGUMENTS HAVE A BIT OF INCONSISTENCY SO THIS HELPER FUNCTION TRANSLATES BETWEEN THEM. EL IS THE RESULT OF GGPLOT2::CALC_ELEMENT()
#' @param el A theme element as returned by `ggplot2::calc_element()`.
#' @return A `grid::gpar` object.
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


#' SOMETIMES WE NEED TO KNOW THE VARIABLE AN AES WAS MAPPED TO. THIS TRIES TO IDENTIFY THIS FROM A BUILT PLOT OBJECT.
#' @param plot A built plot object as returned by `ggplot2::ggplot.build()`.
#' @param aes The text string name of a ggplot2 aesthetic, e.g., "colour".
#' @return The name of a column, as a string (if one could be found).
#' @keywords internal
#' @noRd
.ggplus_mapped_vars_for_aes = function(plot, aes) {

  grab = function(mapping) {
    if(is.null(mapping[[aes]])) { return(NULL) } #IF MAPPING HAS NO CONTENTS, FAIL.

    expr_chr = rlang::as_label(mapping[[aes]]) #FROM THE MAPPING STRING, GET THE VAR NAME AS A LABEL.
    if (rlang::is_atomic(expr)) return(NULL) #TRIGGERED IF A CONSTANT HAS BEEN MAPPED (BAD PRACTICE)
    if(grepl("after_stat\\(|after_scale\\(", expr_chr)) { return(NULL) } #IF THIS IS AN AFTER_* EXPRESSION, JUST SKIP--TOO COMPLICATED.
    return(expr_chr)
  }

  exprs = character(0) #STORAGE OBJ.

  #GRAB PROSPECTIVE NAMES FROM GLOBAL MAPPINGS
  if(!is.null(plot$mapping)) {
    e = grab(plot$mapping);
    if(!is.null(e)) { exprs = c(exprs, e) }
  }

  #DO THE SAME FOR LOCAL MAPPINGS
  for(l in plot$layers) {
    e = grab(l$mapping)
    if(!is.null(e)) { exprs = c(exprs, e) }
  }

  #DID WE FIND ANYTHING?
  exprs = unique(exprs)
  if(!length(exprs)) { return(character(0)) }

  #SOME NAMES MIGHT BE EXPRESSIONS LIKE factor(X), IN WHICH CASE WE NEED TO TRY TO SANITIZE THE EXPRESSION TO GET JUST THE VAR NAME.
  vars = vapply(exprs, function(e) {
    s = gsub("`", "", e)

    #NOT AN EXPRESSION --> END
    if (!grepl("\\(|\\)|\\$|\\[|\\]", s)) {return(s)}

    #SIMPLE EXPRESSIONS LIKE factor(x)
    inside = sub(".*\\(([^()]+)\\).*", "\\1", s)

    inside = trimws(strsplit(inside, ",", fixed = TRUE)[[1]][1])

    #FALL BACK TO EXPRESSION IF WE CAN'T SIMPLIFY IT...
    if(nzchar(inside)) {inside} else {s}
  }, character(1))

  return(unique(vars[nzchar(vars)]))
}


#' SOMETIMES, USERS WILL SPECIFY DATA AND/OR MAPPING ARGUMENTS INSIDE GEOM_PLUS BUT NOT GIVE THEM NAMES. IF INHERIT.AES = FALSE, THESE MAY THEN END UP IN THE WRONG ARGUMENT SLOTS, SO THIS FUNCTION HELPS DETECT WHEN THAT MAY HAVE OCCURRED AND TRY TO CATCH AND PROPERLY PLACE THOSE ARGUMENTS. ALTERNATIVELY, IT HELPS TO CATCH WHEN INHERIT.AES = TRUE AND THESE WERE PROVIDED ANYWAY AND SHOULD INSTEAD BE IGNORED.
#' @param args A list of user arguments given to `geom_plus` via ...
#' @param inherit.aes Logical value as to whether inherit.aes is on or off.
#' @return The same list of arguments, but perhaps with some arguments now named.
#' @keywords internal
#' @noRd
.ggplus_normalize_layer_args = function(args, inherit.aes) {

  if (is.null(args) || !length(args)) { return(args) } #IF THERE ARE NO ARGS, END EARLY

  nms = names(args) #STORAGE OBJ.
  if(is.null(nms)) { nms = rep("", length(args)) } #FAILSAFE STORAGE OBJ

  #FIRST, FIGURE OUT WHO LOOKS LIKE A MAPPING ARGUMENT.
  is_uneval = vapply(args, function(x) inherits(x, "uneval"), logical(1)) #WHO HAS UNEVAL CLASS?

  if(inherit.aes == FALSE) { #IF INHERIT IS FALSE, THERE SHOULD BE A MAPPING...

    idx_map = which(is_uneval &
                      (nms == "" | is.na(nms))) #SEE WHICH QUALIFY AND AREN'T NAMED.

    if(length(idx_map) == 1 &
       !any(nms == "mapping")) { #ONLY DO THIS IF THERE ISN'T ALREADY AN EXPLICITLY NAMED MAPPING ARG.
      args$mapping = args[[idx_map]] #ASSIGN THIS TO MAPPING
      args[[idx_map]] = NULL #BLANK ITS UNNAMED VERSION
      nms = names(args) #REFRESH NAMES OBJ
      message("The first unnamed aes-like input in geom_plus() was interpreted as an input to mapping.")
    } else {
      if(length(idx_map) > 1) {
        args[[idx_map]] = NULL #WIPE THEM ALL OUT
        message("Too many unnamed aes-like inputs in geom_plus() were given. All were ignored.")
      }
      if(length(idx_map) == 1 &
         any(nms == "mapping")) {
        args[[idx_map]] = NULL #WIPE THEM ALL OUT
        message("You provided an unnamed aes-like input to geom_plus() but also a named mapping input. The former was ignored.")
      }
    }
  } else {
    if(any(is_uneval) > 0) {
    args[[which(is_uneval)]] = NULL #IF INHERIT.AES IS TRUE, DESTROY ANY MAPPING ARG AS THIS SHOULD BE IGNORED.
    message("You provided an unnamed aes-like input to geom_plus() but inherit.aes is TRUE, so the former was ignored.")
    }
  }

  #DO THE SAME THING WITH PROSPECTIVE DATA ARGUMENTS.
  is_data_like = function(x) {
    inherits(x, c("data.frame","tbl_df","sf","data.table")) || is.matrix(x)
  }

    is_dat = vapply(args, is_data_like, logical(1)) #WHICH ARGS LOOK LIKE DATA?

    if(inherit.aes == FALSE) {

    unnamed_dat = which(is_dat & (nms == "" | is.na(nms)))

    if(length(unnamed_dat) == 1 &
       !any(nms == "data")) {
      args$data = args[[unnamed_dat]]
      args[[unnamed_dat]] = NULL
      nms = names(args)
      message("The first unnamed dataset-like input in geom_plus() was interpreted as an input to data.")
    } else {
      if(length(unnamed_dat) > 1) {
        message("Too many unnamed dataset-like inputs in geom_plus() were given. All were ignored.")
        args[[unnamed_dat]] = NULL #WIPE THEM ALL OUT
      }
      if(length(unnamed_dat) == 1 &
         any(nms == "data")) {
        message("You provided an unnamed dataset-like input to geom_plus() but also a named data input. The former was ignored.")
        args[[unnamed_dat]] = NULL #WIPE THEM ALL OUT
      }
     }
    } else {
      if(any(is_dat) > 0) {
      args[[which(is_dat)]] = NULL #IF INHERIT.AES IS TRUE, DESTROY ANY DATA ARGS AS THIS SHOULD BE IGNORED.
      message("You provided an unnamed dataset-like input to geom_plus() but inherit.aes is TRUE, so the former was ignored.")
      }
  }


  #IF THERE'S STILL ANYTHING LEFT THAT LOOKS LIKE AN UNEVAL AES() MAPPING ARGUMENT OR A DATA SET, ERROR OUT.
  nms2 = names(args)
  if(is.null(nms2)) { nms2 = rep("", length(args)) }

  leftover_unnamed_uneval = any(vapply(args, inherits, logical(1), "uneval") &
                                  (nms2 == "" | is.na(nms2)))

  if(leftover_unnamed_uneval) {
    stop("geom_plus() found an unnamed aes() argument inside '...'. Explicitly name it using 'mapping = aes(...)' and only provide one call to aes() as an input inside geom_plus() (and only when inherit.aes = FALSE).",
         call. = FALSE)
  }

  leftover_unnamed_ds = any(vapply(args, is_data_like, logical(1)) &
                                  (nms2 == "" | is.na(nms2)))

  if(leftover_unnamed_ds) {
    stop("geom_plus() found an unnamed possible data argument inside '...'. Explicitly name it using 'data = ...' and only provide one explicit data argument as an input inside geom_plus() (and only when inherit.aes = FALSE).",
         call. = FALSE)
  }

  return(args)
}
