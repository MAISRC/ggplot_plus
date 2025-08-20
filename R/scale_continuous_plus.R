#' Internal: Compute “pretty” breaks anchored near endpoints by expanding the limits slightly as necessary.
#' @param lims numeric(2) range
#' @param n target number of breaks
#' @param buffer_frac fraction of span used to judge endpoint proximity
#' @param Return "breaks" or "limits"
#' @keywords internal
#' @noRd
.endpoint_breaks = function(lims, n = 5, buffer_frac = 0.05, Return = c("breaks", "limits")) {

  pretty_fn = scales::pretty_breaks(n)
  original_lo = lims[1]
  original_hi = lims[2]
  lo = original_lo
  hi = original_hi
  span = diff(lims)

  #SPAN COULD BE 0 IF LIMITS COME IN EQUAL FOR SOME REASON. THIS MAKES SURE BUFFER WOULD BE NON 0 SO WE ALWAYS GET BACK MORE THAN 1 VALUE FOR BREAKS.
  if(is.infinite(span) || span == 0) {
    eps = if(is.finite(original_lo) && original_lo != 0) {
      abs(original_lo) * 0.05
      } else { 1 }
    lo = original_lo - eps
    hi = original_hi + eps
    span = hi - lo
  }

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
#' @keywords internal
#' @noRd
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

  ggplot.plus_cached_limits <<- NULL #PRE-ESTABLISH THIS OBJECT FOR REFERENCE

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

  ##WITH TRANS AND TRANSFORM ARGUMENTS, WANT TO CATCH EITHER AND SHUNT TO TRANSFORM, W/ ERROR OUT IF TWO MATCHES.
  if(!is.null(names(named.args)) && length(named.args) > 0) { #IF THERE ARE NAMED ARGUMENTS
    nms = names(named.args)

    #SEE WHICH MATCH TRANS/TRANSFORM
    matches_transform = vapply(nms, function(nm) startsWith("transform", nm), logical(1))
    matches_trans     = vapply(nms, function(nm) startsWith("trans",     nm), logical(1))

    #AMBIGUOUS ONES, ERROR OUT
    matches_either = matches_transform | matches_trans

    if(sum(matches_either) > 1) {
      stop("Multiple arguments could match 'transform'/'trans'. Please supply a single argument for transformation.", call. = FALSE)
    }

    #OTHERWISE, SHUNT TO TRANSFORM FORMALLY.
    if(sum(matches_either) == 1) {
      idx = which(matches_either)[1]
      named.args$transform = named.args[[idx]]
      named.args[idx] = NULL
      warning("A \"transform\" argument was provided. At present, we cannot guarantee that all transformations will work properly with ggplot.plus's scale_*_continuous_plus functions. Consider either pre-transforming the data or using ggplot2's scale functions instead.", call. = FALSE)
    }
  }

  ###--EARLY FAILS--###

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
      if(!is.null(ggplot.plus_cached_limits)) {
        use_cache = ggplot.plus_cached_limits
        ggplot.plus_cached_limits <<- NULL #SAFER THAN REMOVING IT OUTRIGHT.
        return(.endpoint_breaks(use_cache, n = n, buffer_frac = buffer_frac, Return = "limits"))
      } else {
      return(c(0,1)) #DUNNO WHAT THIS SHOULD BE REALLY.
      }
    } else {
      ggplot.plus_cached_limits <<- lims
      return(.endpoint_breaks(lims, n = n, buffer_frac = buffer_frac, Return = "limits"))
    }
  }

  breaks_arg = function(lims) {
    if (is.null(lims) || !is.numeric(lims) || length(lims) != 2 || any(!is.finite(lims)) || identical(lims, c(0,1))) {
      return(numeric())
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

  scale = do.call(ggplot2::continuous_scale, args)

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
#' @keywords internal
#' @noRd
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

  #arg.names = names(user.args) #KEEP IN MIND arg.names WOULD BE OUT OF DATE BY THIS POINT AND SHOULD BE RE-INITIALIZED IF I EVER USE IT AGAIN AFTER THIS POINT.

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


#' Continuous Scales With Guaranteed Breaks Near The Data Limits
#'
#' `scale_x_continuous_plus()` and its kin create continuous scales that try to
#' put a break+label **at, near, or just beyond** both ends of the data range
#' while retaining “pretty” break spacing. This avoids unlabeled endpoints, e.g.
#' data ranging from 2.5–8.5 but breaks of 3, 5, 7 only. Minor breaks are not used.
#' Typical scale arguments beyond `breaks`, `limits`, and a few others can be used
#' just as in `ggplot2`'s scale_*_continuous() functions.
#'
#' @param ... Additional arguments passed to [ggplot2::continuous_scale()].
#'   Do **not** supply `breaks`, `limits`, `minor_breaks`, `n.breaks`, `expand`,
#'   `rescaler`, `call`, or `super` arguments here, as these are are controlled
#'   or deprecated. You may provide `labels`, though they will need to match in
#'   length the number of new breaks exactly, which may require trial and error.
#'   If you pass `trans` (deprecated), the input will be forwarded as `transform`.
#' @param n Desired number of major breaks (a target only; the final count may
#' differ slightly). Default `5`. Should not generally need adjustment.
#' @param buffer_frac Fraction of the data span used to judge whether an endpoint
#'   break is “close enough” to the min/max. Default `0.05` (i.e., within 5% of
#'   the span). Should not generally need adjustment.
#' @param thin_labels Logical. If `TRUE`, every second label (starting with the
#'   second) is replaced by `""`—handy for when breaks look too dense. Ignored
#'   if you provide a custom `labels` argument.
#'
#' @return A ggproto `ScaleContinuous*` object to add with `+`.
#'
#' @section How it works (briefly):
#' The scale computes candidate breaks with [scales::pretty_breaks()] and, if
#' needed, **gently expands** the working scale limits until there’s a break
#' near both ends (within `buffer_frac`). Final `limits` are set from the
#' expanded span, and final `breaks` are clipped back to the trained limits.
#'
#' @examples
#' library(ggplot2)
#'
#' # Endpoint-labeled x and y
#' ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Petal.Length)) +
#'   ggplot2::geom_point() +
#'   scale_x_continuous_plus() +
#'   scale_y_continuous_plus()
#'
#' #' # For comparison
#' ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Petal.Length)) +
#'   ggplot2::geom_point() +
#'   ggplot2::scale_x_continuous() +
#'   ggplot2::scale_y_continuous()
#'
#' # Boxplot: discrete x, continuous y → y-only endpoint-aware breaks
#' ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) +
#'   ggplot2::geom_boxplot() +
#'   scale_y_continuous_plus(n = 6)
#'
#' # Continuous colorbar with endpoint-conscious breaks, though with tweaks
#' ggplot2::ggplot(faithfuld, ggplot2::aes(waiting, eruptions, fill = density)) +
#'   ggplot2::geom_raster() +
#'   scale_fill_continuous_plus(buffer_frac = 0.03, thin_labels = TRUE)
#'
#' @seealso ggplot2’s [ggplot2::scale_x_continuous()],
#'   [scales::pretty_breaks()], and [ggplot2::guide_axis()]
#'
#' @name scale_continuous_plus
#' @aliases scale_x_continuous_plus scale_y_continuous_plus
#'   scale_colour_continuous_plus scale_color_continuous_plus
#'   scale_fill_continuous_plus
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

#' @rdname scale_continuous_plus
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

#AND NOW THE COLOR ONE.
#' @rdname scale_continuous_plus
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
                        aes = c("colour"),
                       scale_name = "colour_continuous_plus",
                       palette = scales::col_numeric(palette = viridis::cividis(256), domain = NULL),
                       n = n,
                       buffer_frac = buffer_frac,
                       thin_labels = thin_labels,
                       guide = guide_arg,
                       super = ggplot2::ScaleContinuous)
}

#AN ALIAS FOR THE COLOR SPELLINGS.
#' @rdname scale_continuous_plus
#' @export
scale_color_continuous_plus = scale_colour_continuous_plus

#' @rdname scale_continuous_plus
#' @export
scale_fill_continuous_plus = function(...,
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
                        aes = c("fill"),
                        scale_name = "colour_continuous_plus",
                        palette = scales::col_numeric(palette = viridis::cividis(256), domain = NULL),
                        n = n,
                        buffer_frac = buffer_frac,
                        thin_labels = thin_labels,
                        guide = guide_arg,
                        super = ggplot2::ScaleContinuous)
}
