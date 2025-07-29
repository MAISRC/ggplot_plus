#THIS FUNCTION TAKES THREE INPUTS--THE CURRENT LIMITS OF A SCALE OF DATA, A DESIRED NUMBER OF TARGET BREAKS, AND A BUFFER FRACTION--AND ATTEMPTS TO DETERMINE IF THERE WILL BE A SCALE BREAK/LABEL AT, NEAR, OR PAST THE MIN AND MAX VALUES OF THE DATA. IF NOT, IT WILL PUSH THE CURRENT LIMITS OUT A LITTLE WAYS UNTIL SUCH A BREAK CAN BE ACHIEVED.
.endpoint_breaks = function(lims, n = 5, buffer_frac = 0.05, Return = c("breaks", "limits")) {

  pretty_fn = scales::pretty_breaks(n)
  original_lo = lims[1]
  original_hi = lims[2]
  lo = original_lo
  hi = original_hi
  span = diff(lims)
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

#THIS IS A FACTORY FUNCTION THAT CAN BE USED TO DIRECT THE continuous_scale() CONSTRUCTOR HOW TO BUILD SPECIFIC INSTANCES OF A GENERAL "PLUS" CONTINUOUS SCALE.
#IT TAKES AS INPUTS MOST OF THE TYPICAL STUFF A CONTINUOUS SCALE TAKES, PLUS SOME ADDITIONAL THINGS, SUCH AS EXACTLY WE WANT TO FIND END-OF-AXIS BREAK POINTS AND WHETHER WE WANT TO THIN LABELS AND SO FORTH.
.make_continuous_plus = function(user.args,
                                 aes, #WHICH AESTHETIC DO WE WANT TO BUILD AN AXIS FOR?
                                guide,
                                position = NULL, #SET TO NULL, OVERRIDE FOR POSITIONAL AXES (X AND Y)
                                super,
                                scale_name,
                                palette = identity,
                                n = 5,
                                buffer_frac = 0.05,
                                thin_labels = FALSE,
                                expand = c(0,0),
                                sec.axis = NULL #DEFAULT TO NOTHING.
                                ) {

  if(!is.list(user.args)) { user.args = as.list(user.args) }

  arg.names = names(user.args) #GET ALL NAMES OF ALL ELEMENTS.
  if(!all(is.na(arg.names))) {
    is_named = nzchar(arg.names) & !is.na(arg.names)
  } else {
    is_named = FALSE
  }

  #BREAK INTO TWO SEPARATE LISTS DEPENDING.
  named.args = user.args[is_named]
  unnamed.args = user.args[!is_named]

  ###--EARLY FAILS--###

  if(!is.null(arg.names) &&
     length(arg.names) > 0 &&
     any(pmatch(arg.names, "transform", nomatch = 0L) > 0)) {
    warning("A \"transform\" argument was provided. At present, we cannot guarantee that all transformations will work properly with ggplot.plus's scale functions. Consider either pre-transforming the data or using ggplot2's scale functions instead.", call. = FALSE)
  }

  #SOME INPUTS ARE FORBIDDEN BECAUSE OF THE FUNCTIONS' OPINIONS. CATCH ANY MATCHING ARGUMENTS AND TRIP A STOP.
  forbidden = c("breaks", "limits", "minor_breaks", "n.breaks", "expand", "rescaler", "call", "super")

  #THEN USE THE PMATCH FUNCTION TO CHECK TO SEE IF ANY PARTIAL MATCHES WOULD TRIP.
  if(!is.null(arg.names) &&
     length(arg.names) > 0 &&
     any(pmatch(arg.names, forbidden, nomatch = 0L) > 0)) {
    stop("The scale_*_continuous_plus() functions are designed to set (minor) breaks and limits for you. In doing so, they suppress any potential expansion. If you want finer control over any of these parameters, use the corresponding ggplot2 scale_*_continuous() function instead. For \"n.breaks\" control, use \"n\" instead. Do not specify arguments that might match \"rescaler\", \"call\", or \"super\"; there is no benefit, and such inputs would be inadvertantly passed to `ggplot2::continuous_scale` internally and cause unintended consequences.", call. = FALSE)
  }

  #AMBIGUITY IN OTHER (NAMED) INPUTS SHOULD ALSO TRIP STOPS.
  label_idx = which(pmatch(arg.names, "labels", nomatch = 0L, duplicates.ok = T) > 0)

  #AMBIGUITY IN LABELS ARGUMENT.
  if(length(label_idx) > 1) {
    stop("Multiple arguments matched \"labels\". Please provide one, unambiguous labels argument.", call. = FALSE)
  }

  #PERFORM THE SAME SAFE SEARCH FOR ONE AMBIGUOUS NAMES ARGUMENT ALSO...
  name_idx_named = which(pmatch(arg.names, "name", nomatch = 0L, duplicates.ok = TRUE) > 0)

  #BUT WE ALSO WANT TO LOOK FOR ANY UNNAMED ARGUMENTS THAT IS LIKELY TO BE AN UNNAMED NAME ARGUMENT.
  unnamed_char1 = which(vapply(unnamed.args, function(x) is.character(x) && length(x) == 1, logical(1))) #IT SHOULD BE A CHARACTER OF LENGTH 1

  #WE ERROR OUT IF EITHER TWO AMBIGUOUS NAMED NAME ARGUMENTS WERE PROVIDED OR AT LEAST ONE NAMED ONE PLUS ONE UNNAMED ONE.
  if((length(name_idx_named) > 0 && length(unnamed_char1) > 0) ||
     length(name_idx_named) > 1) {
    stop("Too many arguments that could be interpretted as a \"name\" argument were provided. Please provide one, umambiguous name argument.", call. = TRUE)
  }

  ###--INDIVIDUAL ARGUMENT LOGIC--###

  ##--LABELS ARGUMENTS--##
  #THE CODE IGNORES thin_labels = TRUE WHEN THERE'S A LABELS ARGUMENT; LET'S WARN ABOUT THAT.
  label_idx = which(pmatch(names(named.args), "labels", nomatch = 0L, duplicates.ok = T) > 0) #GET THE INDEX AGAIN IN CASE IT'S CHANGED.
  if(length(label_idx) == 1 &&
     thin_labels) {
    warning("thin_labels was set to TRUE, but a custom label argument was provided, so the former was ignored.")
  }

  #OTHERWISE, STRIP OUT LABELS TO PASS EXPLICITLY
  if(length(label_idx) == 1 &&
      label_idx > 0) {
    arg.names = names(named.args) #GET NAMES AGAIN IN CASE POSITIONS HAVE CHANGED.
    label_name = arg.names[label_idx]
    labels_arg = named.args[[label_name]]
    named.args[[label_name]] = NULL
  } else if (thin_labels) { #THIN AUTOMATIC LABELS IF APPROPRIATE.
    labels_arg = function(b) {
      real = which(!is.na(b))
      b[real[seq(2, length(real), 2)]] = ""
      b
    }
  } else {
    labels_arg = ggplot2::waiver()
  }

  ##--BREAKS AND LIMITS ARGUMENTS--##
  #SET BREAKS USING ENDPOINT_BREAKS, DISCARDING ANY CENSORED BREAKS SO THAT CUSTOM LABELS CAN MORE EASILY MATCH.
  #DURING PROBING, ESP. ON GRAPHS WITH ONE CONTINUOUS AND ONE DISCRETE AXIS, GGPLOT2 WILL SOMETIMES MAKE THE LIMITS NULL TO SEE WHAT HAPPENS, SO WE HAVE TO CACHE ACCEPTABLE LIMITS AS "ONE-TIME USE" TO GET PAST THOSE INSTANCES.
  limits_arg = function(lims) {
    if (is.null(lims) || !is.numeric(lims) || length(lims) != 2 || any(!is.finite(lims))) {
      if(!is.null(cached_limits)) {
        use_cache = cached_limits
        rm(cached_limits)
        return(.endpoint_breaks(use_cache, n = n, buffer_frac = buffer_frac, Return = "limits"))
      } else {
      return(c(0,1)) #DUNNO WHAT THIS SHOULD BE REALLY.
      }
    } else {
      cached_limits <<- lims
      return(.endpoint_breaks(lims, n = n, buffer_frac = buffer_frac, Return = "limits"))
    }
  }

  breaks_arg = function(lims) {
    if (is.null(lims) || !is.numeric(lims) || length(lims) != 2 || any(!is.finite(lims)) || identical(lims, c(0,1))) {
      return(numeric())  # Or numeric(0), but this might be safer
    } else {
      scales::discard(.endpoint_breaks(lims, n = n, buffer_frac = buffer_frac, Return = "breaks"), lims)
    }
  }

  ##--NAME ARGUMENT--##
  #IF THERE IS JUST THE ONE NAME ARGUMENT, GRAB IT AND GO.
  name_idx_named = which(pmatch(names(named.args), "name", nomatch = 0L, duplicates.ok = TRUE) > 0)
  if (length(name_idx_named) == 1) {
    arg.names = names(named.args) #GET AGAIN IN CASE POSITIONS HAVE CHANGED.
    name_name = arg.names[name_idx_named]
    name_arg = named.args[[name_name]]
    named.args[[name_name]] = NULL
  } else if (length(unnamed_char1) == 1) { #WE ASSUME ANY SINGLE UNNAMED CHARACTER OF LENGTH 1 IS AN UNNAMED NAME ARGUMENT (OPINIONATED)
    name_idx = which(vapply(unnamed.args, function(x) is.character(x) && length(x) == 1, logical(1)))
    name_arg = unnamed.args[[name_idx]]
    unnamed.args[[name_idx]] = NULL
  } else {
    warning(sprintf("No obvious name argument was provided for the %s scale, so its name will default to ggplot's default, which may lack desirable characteristics. Consider adding a descriptive, human-readable, and properly formatted name containing units, if applicable.", aes), call. = FALSE) #TRIGGER A THOUGHTFUL WARNING TO EDUCATE.
    name_arg = ggplot2::waiver() #DEFAULT TO GGPLOT2'S DEFAULT NAMING SYSTEM OTHERWISE.
  }

##--ASSEMBLING DO.CALL ARGUMENTS LIST.

  args = list(aesthetics = aes,
              scale_name = scale_name,
              palette = palette,
              name = name_arg,
              breaks = breaks_arg,
              minor_breaks = ggplot2::waiver(),
              labels = labels_arg,
              limits = limits_arg,
              expand = expand,
              guide = guide,
              super = super)

  #ADD POSITION AND SEC.AXIS CONDITIONALLY.
  if (!is.null(position)) {
    args$position = position
  }

  #ONLY JOIN IN USER ARGS IF THERE ARE ANY LEFT
  if(length(unnamed.args) > 0) {
    args = c(args, unnamed.args)
  }
  if(length(named.args) > 0) {
    args = c(args, named.args)
  }

  scale = do.call(continuous_scale, args)

  #THIS MIMICS INTERNAL GGPLOT2 LOGIC FOR HOW TO TACK ON A SECONDARY AXIS POST-CONTINUOUS_SCALE (see: https://rdrr.io/cran/ggplot2/src/R/axis-secondary.R#sym-set_sec_axis)
  if(!inherits(sec.axis, "waiver")  &&
     !is.null(sec.axis)) {
    if(inherits(sec.axis, "formula")) {
      sec.axis = ggplot2::sec_axis(sec.axis)
    }
    if (!inherits(sec.axis, "AxisSecondary")) {
      stop("Secondary axes must be specified using `sec_axis()` or a formula.", call. = FALSE)
    }

    scale$secondary.axis = sec.axis
  }

  return(scale)
}

#THIS FUNCTION BUILDS A FACTORY FOR BUILDING "PLUS" VERSIONS OF THE SCALE_X/Y_CONTINUOUS FUNCTIONS IN GGPLOT. THE FUNCTIONS THEMSELVES WILL CALL THIS ONE UNIFIED POSITIONAL FACTORY FOR CONCISENESS.
.make_positional_scale_plus = function(axis = c("x", "y"),
                                       ...,
                                       n = 5,
                                       buffer_frac = 0.05,
                                       thin_labels = FALSE) {
  axis = match.arg(axis) #WHICH AXIS WE MAKING?

  user.args = list(...) #UNPACK USER ARGS
  arg.names = names(user.args) #GET THEIR NAMES

  ##ALLOW USERS TO SPECIFY A DIFFERENT POSITION ARGUMENT
  pos_idx = which(pmatch(arg.names, "position", nomatch = 0L) > 0) #FIND A PARTIAL MATCH FOR POSITION

  if(length(pos_idx) > 1) {
    stop("Multiple arguments match 'position'. Please specify a single, unambiguous position argument.", call. = FALSE)
  }

  if(length(pos_idx) == 1) {
    if(!user.args[[pos_idx]] %in% c("left", "top", "bottom", "right")) {
      stop("An invalid character string was provided for \"position\". Only \"top\", \"bottom\", \"right\", and \"left\" are valid values.", call. = FALSE)
    }

    position_arg = user.args[[pos_idx]]
    user.args[[pos_idx]] = NULL
  } else {
    position_arg = if (axis == "x") "bottom" else "left"
  }

  ##ALLOW USERS TO SPECIFY A DIFFERENT SEC.AXIS ARGUMENT
  sec_idx = which(pmatch(arg.names, "sec.axis", nomatch = 0L) > 0) #FIND A PARTIAL MATCH FOR SEC.AXIS

  if(length(sec_idx) > 1) {
    stop("Multiple arguments match \"sec.axis\". Please specify a single, unambiguous sec.axis argument.", call. = FALSE)
  }
  if(length(sec_idx) == 1) {
    sec_arg = user.args[[sec_idx]]
    user.args[[sec_idx]] = NULL
  } else {
    sec_arg = NULL
  }

  ##REJECT A USER-PROVIDED GUIDE ARGUMENT--NOT RELEVANT FOR POSITIONAL SCALES.

  guide_idx = which(pmatch(arg.names, "guide", nomatch = 0L) > 0) #FIND A PARTIAL MATCH FOR GUIDE
  if(length(guide_idx) > 0) {
    warning("A guide argument was provided, but positional scales always use guide_axis(). This argument was ignored.", call. = FALSE)
    user.args[[guide_idx]] = NULL
  }

  .make_continuous_plus(
      user.args = user.args,
      aes = axis,
      scale_name = paste0(axis, "_continuous_plus"),
      palette = identity,
      n = n,
      buffer_frac = buffer_frac,
      thin_labels = thin_labels,
      guide = ggplot2::guide_axis(check.overlap = FALSE),
      position = position_arg,
      super = ggplot2::ScaleContinuousPosition,
      sec.axis = sec_arg
  )
}

#' Continuous Scales with Breaks Anchored Close to Data Limits
#'
#' These scale functions create continuous axis scales (or colorbars) that use a modified break-finding algorithm compared to the one used by `ggplot2::scale_*_continuous()`. Specifically, the algorithm aims to ensure that breakpoints are still visually "pretty" but that breakpoints exist at, near, or just past the range of the provided data on both sides. This helps avoid situations where endpoints of a scale are essentially unlabeled.
#'
#' @param ... Additional arguments passed to `continuous_scale()`, as one might provide to `ggplot2::scale_*_continuous()`. Must not include `breaks` or `limits`; these are handled internally. You may still supply custom `labels`, if desired, so long as their length matches the length of the final breaks exactly. As such, you may want to first call this function once without specifying labels to see how many will be needed.
#' @param n Desired number of breaks. Passed to an internal `pretty_breaks`-based algorithm. Defaults to 5. The final number of breaks may vary slightly.
#' @param buffer_frac A fraction of the data range used to determine how close the endpoint breaks must be to the data limits. Defaults to `0.05` (i.e., within 5% of the data range).
#' @param thin_labels Logical. If `TRUE`, replaces every other label (starting with the second) with an empty string. Useful for reducing label clutter when breaks are dense/numerous.
#'
#' @return A `ScaleContinuous` ggproto object that can be added to a ggplot.
#'
#' @examples
#' ggplot2::ggplot(iris, ggplot2::aes(x = Sepal.Length, y = Petal.Length)) +
#'   ggplot.plus::geom_plus("point") +
#'   ggplot.plus::scale_x_continuous_plus()
#'
#' @seealso
#'   \code{\link[ggplot2]{scale_x_continuous}},
#'   \code{\link[scales]{pretty_breaks}},
#'   \code{\link[ggplot.plus]{scale_y_continuous_plus}},
#'   \code{\link[ggplot.plus]{scale_colour_continuous_plus}},
#'   \code{\link[ggplot.plus]{scale_color_continuous_plus}},
#'   \code{\link[ggplot.plus]{scale_fill_continuous_plus}}
#'
#' @export
scale_x_continuous_plus = function(...,
                                   n = 5,
                                   buffer_frac = 0.05,
                                   thin_labels = FALSE) {

  .make_positional_scale_plus("x", #MAKE THE X VERSION
                             ...,
                             n = n,
                             buffer_frac = buffer_frac,
                             thin_labels = thin_labels)
}

#' @rdname scale_x_continuous_plus
#' @export
scale_y_continuous_plus = function(...,
                                   n = 5,
                                   buffer_frac = 0.05,
                                   thin_labels = FALSE) {

  .make_positional_scale_plus("y", #MAKE THE Y VERSION
                             ...,
                             n = n,
                             buffer_frac = buffer_frac,
                             thin_labels = thin_labels)
}

#' @rdname scale_x_continuous_plus
#' @export
scale_colour_continuous_plus = function(...,
                                   n = 5,
                                   buffer_frac = 0.05,
                                   thin_labels = FALSE) {

  #ALLOW USERS TO OVERRIDE WITH A DIFFERENT GUIDE (OR SUPPRESS)
  user.args = list(...) #UNPACK USER ARGS
  arg.names = names(user.args) #GET THEIR NAMES
  guide_idx = which(pmatch(arg.names, "guide", nomatch = 0L) > 0) #FIND A PARTIAL MATCH FOR GUIDE

  if(length(guide_idx) > 1) {
    stop("Multiple arguments match \"guide\". Please specify a single, unambiguous guide argument.", call. = FALSE)
  }
  if(length(guide_idx) == 1) {
    guide_arg = user.args[[guide_idx]]
    user.args[[guide_idx]] = NULL
  } else {
    guide_arg = ggplot2::guide_colorbar()
  }

  ##REJECT A USER-PROVIDED SEC.AXIS ARGUMENT--NOT RELEVANT FOR NON-POSITIONAL SCALES.
  sec_idx = which(pmatch(arg.names, "sec.axis", nomatch = 0L) > 0) #FIND A PARTIAL MATCH FOR GUIDE
  if(length(sec_idx) > 0) {
    warning("A sec.axis argument was provided, but this is not relevant for non-positional scales. This argument was ignored.", call. = FALSE)
    user.args[[sec_idx]] = NULL
  }

  .make_continuous_plus(user.args = user.args,
                        aes = c("colour", "fill"), #SINCE COLOR AND FILL ARE GOING TO HAVE THE EXACT SAME INPUTS, I CAN JUST MAKE A SINGLE FACTORY FOR BOTH AND ALIAS THE OTHERS.
                       scale_name = "colour_continuous_plus",
                       palette = scales::col_numeric(palette = viridis::cividis(256), domain = NULL),
                       n = n,
                       buffer_frac = buffer_frac,
                       thin_labels = thin_labels,
                       guide = guide_arg,
                       super = ScaleContinuous)
}

#AND HERE ARE THOSE ALIASES. TECHNICALLY, COLOR WORKS FOR FILL AND VICE VERSA BUT THIS IS IDIOMATIC.
#' @rdname scale_x_continuous_plus
#' @export
scale_color_continuous_plus = scale_colour_continuous_plus

#' @rdname scale_x_continuous_plus
#' @export
scale_fill_continuous_plus = scale_colour_continuous_plus
