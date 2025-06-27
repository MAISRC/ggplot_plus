#' @importFrom ggplot2 ggplot_add
NULL
#' Find Pretty Breaks for Continuous Axes in ggplot While Ensuring End Labels
#'
#' This function attempts to find a set of breaks for a continuous variable such that there aren't too many breaks, the breaks are "pretty" values where possible, and breaks exist at or near to the range values of the variable. It is essential `pretty_breaks()` from the `scales` package except that it is more opinionated about needing breaks at or near both ends of the data range. If needed, the limits of the axis are expanded slightly to yield a new break just past the range of the data.
#'
#' @param data The single vector of strictly numeric data to find pretty breaks for. Required.
#' @param n A length-1 numeric value for the "target" number of breaks to create. Defaults to 5.
#' @param buffer_frac A length-1 numeric value corresponding to how close the end breaks must be to the end of the data for new breaks to not be added. Defaults to 0.05 (5%).
#' @return Returns a named lists containing breaks and limits to use for the `data` provided, to be used in a `scale_*_continuous()` function in a `ggplot2` call.
#' @examples
#' cont_breaks_plus(iris$Sepal.Length)
#' @export
cont_breaks_plus = function(data,
                                   n = 5,
                                   buffer_frac = 0.05) {

  breaks_fn = scales::pretty_breaks(n = n) #CREATE A BASE BREAKS FUNCTION AIMING FOR A SPECIFIC NUMBER OF BREAKS, BY DEFAULT 5.

  data_range = range(data, na.rm = TRUE) #THE ORIGINAL RANGE OF THE DATA
  original_lower = data_range[1] #THE ORIGINAL UPPER AND LOWER VALUES
  original_upper = data_range[2]
  lower = original_lower #THE CHANGING UPPER AND LOWER VALUES, AS WE NEED TO EXPAND THE LIMITS TO GET LABELS BY THE EDGES OF THE GRAPH
  upper = original_upper
  span = diff(data_range) #THE LENGTH OF THE RANGE BTW THESE VALUES.

  max_iter = 50 #SHOULD BE OVERKILL.

  #TRY A SET NUMBER OF TIMES...
  for (i in seq_len(max_iter)) {
    brks = breaks_fn(c(lower, upper)) #A FIRST ATTEMPT
    step = mean(diff(brks)) #GET THE STEP LENGTH (MEAN IF THEY ARE UNEVEN FOR ANY REASON)

    #ARE THERE LABELS WITHIN X% OF THE END OF THE SPECTRUM?
    has_low = any(abs(brks - original_lower) < buffer_frac * span) || any(brks < original_lower)
    has_high = any(abs(brks - original_upper) < buffer_frac * span) || any(brks > original_upper)

    #IF THERE IS, WE JUST RETURN THE BREAKS AND LIMITS AS SET BY THE DATA AND PRETTY_BREAKS, WITH A LITTLE PADDING ON THE LIMITS FOR SAFETY.
    if (has_low && has_high) {
      return(list(breaks = brks, limits = c((min(brks)-(buffer_frac*span)),
                                            (max(brks)+(buffer_frac*span)))))
    }

    #IF NOT, WE ARTIFICIALLY DECREASE THE LOWER/UPPER VALUES BY ONE STEP AND THEN REPEAT THE PROCESS UNTIL WE SUCCEED.
    if (!has_low) lower = lower - step
    if (!has_high) upper = upper + step
  }

  #EITHER WAY, MAKE SURE WE COME OUT WITH BREAKS AND LIMITS EVEN IF THE ABOVE FAILS TO FIND A SOLUTION IN X TRIES.
  brks = breaks_fn(c(lower, upper))
  #WE BUILD IN A LITTLE BUFFER REGION TO THE LIMITS SO THAT WE HOPEFULLY DON'T CUT OFF BINS ON GRAPHS LIKE HISTOGRAMS AND THE LIKE.
  list(breaks = brks, limits = c((min(brks)-(buffer_frac*span)),
                                 (max(brks)+(buffer_frac*span))))
}


#' Find Pretty Breaks for a Continuous X axis in ggplot While Ensuring End Labels
#'
#' This function attempts to find a set of breaks for a continuous variable mapped to the x axis of a ggplot graph such that there aren't too many breaks, the breaks are "pretty" values where possible, and breaks exist at or near to the range values of the variable. It uses `cont_breaks_plus()` to do this--see there for more information.
#'
#' @param ... Standard inputs normally given to `scale_x_continuous()`. Must not include `breaks` or `limits` or an error will be returned, as the function attempts to circumvent the need to specify prettier breaks or appropriate limits.
#' @param n A length-1 numeric value for the "target" number of breaks to create. Defaults to 5. Passed to `cont_breaks_plus()` internally.
#' @param buffer_frac A length-1 numeric value corresponding to how close the end breaks must be to the end of the data for new breaks to not be added. Defaults to 0.05 (5%). A length-1 numeric value for the "target" number of breaks to create. Defaults to 5. Passed to `cont_breaks_plus()` internally.
#' @param thin_labels Should every other label (starting with the second) be replaced with an empty string? Defaults to FALSE. Change to TRUE to enable. Useful for when the number of breaks/labels is high enough that the axis feels "overlabeled" in a way that might contribute to excess cognitive load.
#' @return Returns a list of class `"scale_x_cont_plus"`, which will trigger the `ggplot_add` method by the same name to trigger the axis breaks reconfiguration.
#' @examples
#' ggplot(iris, aes(x=Sepal.Length, y=Petal.Length)) + geom_plus(geom = "point") + scale_x_continuous_plus()
#' @export
scale_x_continuous_plus = function(...,
                                 n = 5,
                                 buffer_frac = 0.05,
                                 thin_labels = FALSE) {
  extra = list(...)

  if(!is.null(extra) && length(extra) > 0 && is.character(extra[[1]]) && is.null(names(extra[[1]]))) {
    names(extra)[1] = "name"  # assume title if unnamed character string
  }

  if(any(names(extra) %in% c("breaks", "limits"))) {

    extra = extra[!names(extra) %in% c("breaks", "limits")]

    warning("The purpose of these functions is to circumvent the need to provide breaks and limits that work for your data, so your breaks/limits inputs were ignored. Use ggplot's default scale functions to set these parameters manually.")

  }

  structure(list(n = n,
                 buffer_frac = buffer_frac,
                 thin_labels = thin_labels,
                 extra_args = extra),
            class = "scale_x_cont_plus")
}

#' Find Pretty Breaks for a Continuous Y axis in ggplot While Ensuring End Labels
#'
#' This function attempts to find a set of breaks for a continuous variable mapped to the y axis of a ggplot graph such that there aren't too many breaks, the breaks are "pretty" values where possible, and breaks exist at or near to the range values of the variable. It uses `cont_breaks_plus()` to do this--see there for more information.
#'
#' @param ... Standard inputs normally given to `scale_y_continuous()`. Must not include `breaks` or `limits` or an error will be returned, as the function attempts to circumvent the need to specify prettier breaks or appropriate limits.
#' @param n A length-1 numeric value for the "target" number of breaks to create. Defaults to 5. Passed to `cont_breaks_plus()` internally.
#' @param buffer_frac A length-1 numeric value corresponding to how close the end breaks must be to the end of the data for new breaks to not be added. Defaults to 0.05 (5%). A length-1 numeric value for the "target" number of breaks to create. Defaults to 5. Passed to `cont_breaks_plus()` internally.
#' @param thin_labels Should every other label (starting with the second) be replaced with an empty string? Defaults to FALSE. Change to TRUE to enable. Useful for when the number of breaks/labels is high enough that the axis feels "overlabeled" in a way that might contribute to excess cognitive load.
#' @return Returns a list of class `"scale_y_cont_plus"`, which will trigger the `ggplot_add` method by the same name to trigger the axis breaks reconfiguration.
#' @examples
#' ggplot(iris, aes(x=Sepal.Length, y=Petal.Length)) + geom_plus(geom = "point") + scale_y_continuous_plus()
#' @export
scale_y_continuous_plus = function(...,
                                 n = 5,
                                 buffer_frac = 0.05,
                                 thin_labels = FALSE) {
  extra = list(...)

  if(!is.null(extra) && length(extra) > 0 && is.character(extra[[1]]) && is.null(names(extra[[1]]))) {
    names(extra)[1] = "name"  # assume title if unnamed character string
  }

  if(any(names(extra) %in% c("breaks", "limits"))) {

    extra = extra[!names(extra) %in% c("breaks", "limits")]

    warning("The purpose of these functions is to circumvent the need to provide breaks and limits that work for your data, so your breaks/limits inputs were ignored. Use ggplot's default scale functions to set these parameters manually.")

  }

  structure(list(n = n,
                 buffer_frac = buffer_frac,
                 thin_labels = thin_labels,
                 extra_args = extra),
            class = "scale_y_cont_plus")
}

#' Find Pretty Breaks for a Continuous Fill axis in ggplot While Ensuring End Labels
#'
#' This function attempts to find a set of breaks for a continuous variable mapped to the fill aesthetic of a ggplot graph such that there aren't too many breaks, the breaks are "pretty" values where possible, and breaks exist at or near to the range values of the variable. It uses `cont_breaks_plus()` to do this--see there for more information.
#'
#' @param ... Standard inputs normally given to `scale_fill_continuous()`. Must not include `breaks` or `limits` or an error will be returned, as the function attempts to circumvent the need to specify prettier breaks or appropriate limits.
#' @param n A length-1 numeric value for the "target" number of breaks to create. Defaults to 5. Passed to `cont_breaks_plus()` internally.
#' @param buffer_frac A length-1 numeric value corresponding to how close the end breaks must be to the end of the data for new breaks to not be added. Defaults to 0.05 (5%). A length-1 numeric value for the "target" number of breaks to create. Defaults to 5. Passed to `cont_breaks_plus()` internally.
#' @param thin_labels Should every other label (starting with the second) be replaced with an empty string? Defaults to FALSE. Change to TRUE to enable. Useful for when the number of breaks/labels is high enough that the axis feels "overlabeled" in a way that might contribute to excess cognitive load.
#' @return Returns a list of class `"scale_fill_cont_plus"`, which will trigger the `ggplot_add` method by the same name to trigger the axis breaks reconfiguration.
#' @examples
#' ggplot(iris, aes(x=Sepal.Length, y=Petal.Length)) + geom_plus(geom = "point") + scale_fill_continuous_plus()
#' @export
scale_fill_continuous_plus = function(...,
                                   n = 5,
                                   buffer_frac = 0.05,
                                   thin_labels = FALSE) {
  extra = list(...)

  if(!is.null(extra) && length(extra) > 0 && is.character(extra[[1]]) && is.null(names(extra[[1]]))) {
    names(extra)[1] = "name"  # assume title if unnamed character string
  }

  if(any(names(extra) %in% c("breaks", "limits"))) {

    extra = extra[!names(extra) %in% c("breaks", "limits")]

    warning("The purpose of these functions is to circumvent the need to provide breaks and limits that work for your data, so your breaks/limits inputs were ignored. Use ggplot's default scale functions to set these parameters manually.")

  }

  structure(list(n = n,
                 buffer_frac = buffer_frac,
                 thin_labels = thin_labels,
                 extra_args = extra),
            class = "scale_fill_cont_plus")
}

#' Find Pretty Breaks for a Continuous Color axis in ggplot While Ensuring End Labels
#'
#' This function attempts to find a set of breaks for a continuous variable mapped to the color aesthetic of a ggplot graph such that there aren't too many breaks, the breaks are "pretty" values where possible, and breaks exist at or near to the range values of the variable. It uses `cont_breaks_plus()` to do this--see there for more information.
#'
#' @param ... Standard inputs normally given to `scale_color_continuous()`. Must not include `breaks` or `limits` or an error will be returned, as the function attempts to circumvent the need to specify prettier breaks or appropriate limits.
#' @param n A length-1 numeric value for the "target" number of breaks to create. Defaults to 5. Passed to `cont_breaks_plus()` internally.
#' @param buffer_frac A length-1 numeric value corresponding to how close the end breaks must be to the end of the data for new breaks to not be added. Defaults to 0.05 (5%). A length-1 numeric value for the "target" number of breaks to create. Defaults to 5. Passed to `cont_breaks_plus()` internally.
#' @param thin_labels Should every other label (starting with the second) be replaced with an empty string? Defaults to FALSE. Change to TRUE to enable. Useful for when the number of breaks/labels is high enough that the axis feels "overlabeled" in a way that might contribute to excess cognitive load.
#' @return Returns a list of class `"scale_color_cont_plus"`, which will trigger the `ggplot_add` method by the same name to trigger the axis breaks reconfiguration.
#' @examples
#' ggplot(iris, aes(x=Sepal.Length, y=Petal.Length)) + geom_plus(geom = "point") + scale_color_continuous_plus()
#' @export
scale_color_continuous_plus = function(...,
                                      n = 5,
                                      buffer_frac = 0.05,
                                      thin_labels = FALSE) {
  extra = list(...)

  if(!is.null(extra) && length(extra) > 0 && is.character(extra[[1]]) && is.null(names(extra[[1]]))) {
    names(extra)[1] = "name"  # assume title if unnamed character string
  }

  if(any(names(extra) %in% c("breaks", "limits"))) {

    extra = extra[!names(extra) %in% c("breaks", "limits")]

    warning("The purpose of these functions is to circumvent the need to provide breaks and limits that work for your data, so your breaks/limits inputs were ignored. Use ggplot's default scale functions to set these parameters manually.")

  }

  structure(list(n = n,
                     buffer_frac = buffer_frac,
                     thin_labels = thin_labels,
                     extra_args = extra),
                class = "scale_color_cont_plus")
}

#' @export
scale_colour_continuous_plus = scale_color_continuous_plus #ALIAS



#' Add A scale_x_cont_plus-generated X axis Gradation to a ggplot
#'
#' This method defines how objects of class `scale_x_continuous_plus`, added by the function of the same name, are added to a ggplot2 plot using the `+` operator.
#' It ensures that the new, "pretty" breaks, now successfully anchored at or near the range values for the data in question, are added to the plot's x axis.
#'
#' @param object An object of class `scale_x_continuous_plus`, created by `scale_x_continuous_plus()`, containing user-provided arguments (if any) or else pre-defined default values that find a set of "pretty" breaks that encompass the full range of values on the axis and that expand the limits of the axis slightly, if needed, to accomplish this.
#' @param plot A ggplot object to which the new x axis scale should be added.
#' @param name Internal name used by ggplot2 when adding the layer.
#'
#' @return A ggplot object with the x axis breaks and limits redefined.
#' @export
ggplot_add.scale_x_cont_plus = function(object, plot, name) {

  #CHECK TO SEE IF X WAS EXPLICITLY MAPPED.
  if (!is.null(plot$mapping$x)) {
    x_expr = rlang::as_name(plot$mapping$x)
    x_data = plot$data[[x_expr]] #IF SO, GRAB THE X DATA EASILY FROM THE PLOT DATA.
  } else {
    #OTHERWISE, FAKE-BUILD THE PLOT...
    built = suppressMessages(ggplot2:::ggplot_build.ggplot(plot))
    x_data = NULL
    #GO THRU THE PLOT'S DATA LAYERS TO SEE IF WE CAN FIND SOMETHING CALLED X
    for (layer in built$data) {
      if ("x" %in% names(layer)) {
        #IF WE FIND ONE AND IT'S NUMERIC, LET'S GRAB THAT.
        candidate = layer$x
        if (is.numeric(candidate)) {
          x_data = candidate
          break
        }
      }
    }
    #IF WE COME UP EMPTY, LET'S ABORT MISSION WITH A WARNING.
    if (is.null(x_data)) {
      warning("scale_x_continuous_plus() could not locate a x-axis variable.")
      return(plot)  # Or just skip adding breaks
    }
  }

  #GATE TO ENSURE THE VARIABLE IS ACTUALLY NUMERIC--OTHERWISE, RETURN AN APPROPRIATE WARNING AND ABORT. THIS HAS THE ADDED BENEFIT OF PROTECTING AGAINST coord_flip() AS WELL AND IS MORE DEFENSIVE OVERALL.
  if(is.numeric(x_data) || inherits(x_data, "Date")) {

  res = cont_breaks_plus(x_data,
                       n = object$n,
                       buffer_frac = object$buffer_frac) #CALL OUR NEW BREAKS FUNCTION

  #IF X WASN'T EXPLICITLY MAPPED BUT INSTEAD DERIVED, BUT A NAME WAS PROVIDED FOR IT BY THE USER, MAKE SURE THAT GETS IN THERE.
  if(!"x" %in% names(plot$mapping)) {
    if(!is.null(object$extra_args$name)) {
      plot$labels$x = object$extra_args$name #STUFF IT IN BY FORCE.
      object$extra_args$name = NULL #WIPE THIS OUT.
    }
  }

  #ON BINNED GRAPHS LIKE HISTOGRAMS, IT'S PROBABLY NOT A GREAT IDEA TO ENFORCE THE LIMITS BECAUSE IT'S HARD TO SET THOSE TO WORK FOR BINS...
  built = suppressMessages(ggplot2:::ggplot_build.ggplot(plot)) #FAKE BUILD THE PLOT.
  #SEE IF ANY OF THE LAYERS LOOK LIKE THEY'D BE BINNED.
  is.binned = any(sapply(built$plot$layers, function(layer) {
    inherits(layer$stat, "StatBin") || inherits(layer$stat, "StatBin2d")
  }))

  #IF ANY ARE BINNED, SUPPRESS THE LIMITS
  breaks_limits = list(breaks = res$breaks, limits = res$limits)
  if(is.binned) { breaks_limits = list(breaks = res$breaks) }

  #IF THE USER HAS REQUESTED THINNED AXIS LABELS, WE SET THOSE TOO BY TAKING THE BREAKS AND REPLACING EVERY EVEN-INDEXED ONE WITH AN EMPTY STRING.
  if(object$thin_labels == TRUE) {
    tmp = res$breaks
    tmp[seq(from = 2, to = length(tmp), by = 2)] = ""
    breaks_limits$labels = tmp
  }

  plot + do.call(scale_x_continuous,
               c(breaks_limits,
                 object$extra_args)) #CALL SCALE_X_CONTINUOUS AS NORMAL EXCEPT OVERRIDE IN THE NEW BREAKS AND LIMITS.
  } else {
    warning("The x scale is not numeric, so your scale_x_continuous_plus() command was ignored.")
    return(plot)
  }

}

#' Add A scale_y_cont_plus-generated Y axis Gradation to a ggplot
#'
#' This method defines how objects of class `scale_y_continuous_plus`, added by the function of the same name, are added to a ggplot2 plot using the `+` operator.
#' It ensures that the new, "pretty" breaks, now successfully anchored at or near the range values for the data in question, are added to the plot's y axis.
#'
#' @param object An object of class `scale_y_continuous_plus`, created by `scale_y_continuous_plus()`, containing user-provided arguments (if any) or else pre-defined default values that find a set of "pretty" breaks that encompass the full range of values on the axis and that expand the limits of the axis slightly, if needed, to accomplish this.
#' @param plot A ggplot object to which the new y axis scale should be added.
#' @param name Internal name used by ggplot2 when adding the layer.
#'
#' @return A ggplot object with the y axis breaks and limits redefined.
#' @export
ggplot_add.scale_y_cont_plus = function(object, plot, name) {

  #CHECK TO SEE IF Y WAS EXPLICITLY MAPPED.
  if (!is.null(plot$mapping$y)) {
    y_expr = rlang::as_name(plot$mapping$y)
    y_data = plot$data[[y_expr]] #IF SO, GRAB THE Y DATA EASILY FROM THE PLOT DATA.
  } else {
    #OTHERWISE, FAKE-BUILD THE PLOT...
    built = suppressMessages(ggplot2:::ggplot_build.ggplot(plot))
    y_data = NULL
    #GO THRU THE PLOT'S DATA LAYERS TO SEE IF WE CAN FIND SOMETHING CALLED Y
    for (layer in built$data) {
      if ("y" %in% names(layer)) {
        #IF WE FIND ONE AND IT'S NUMERIC, LET'S GRAB THAT.
        candidate = layer$y
        if (is.numeric(candidate)) {
          y_data = candidate
          break
        }
      }
    }
    #IF WE COME UP EMPTY, LET'S ABORT MISSION WITH A WARNING.
    if (is.null(y_data)) {
      warning("scale_y_continuous_plus() could not locate a y-axis variable.")
      return(plot)  # Or just skip adding breaks
    }
  }

  #GATE TO ENSURE THE VARIABLE IS ACTUALLY NUMERIC--OTHERWISE, RETURN AN APPROPRIATE WARNING AND ABORT.
  if(is.numeric(y_data) || inherits(y_data, "Date")) {

  #CONTINUE ON TO CALCULATE NEW LIMITS AND BREAK.
  res = cont_breaks_plus(y_data,
                       n = object$n,
                       buffer_frac = object$buffer_frac)

  #IF Y WASN'T EXPLICITLY MAPPED BUT INSTEAD DERIVED, BUT A NAME WAS PROVIDED FOR IT BY THE USER, MAKE SURE THAT GETS IN THERE.
  if(!"y" %in% names(plot$mapping)) {
    if(!is.null(object$extra_args$name)) {
      plot$labels$y = object$extra_args$name #STUFF IT IN BY FORCE.
      object$extra_args$name = NULL #WIPE THIS OUT.
    }
  }

  #ON BINNED GRAPHS LIKE HISTOGRAMS, IT'S PROBABLY NOT A GREAT IDEA TO ENFORCE THE LIMITS BECAUSE IT'S HARD TO SET THOSE TO WORK FOR BINS...
  built = suppressMessages(ggplot2:::ggplot_build.ggplot(plot)) #FAKE BUILD THE PLOT.
  #SEE IF ANY OF THE LAYERS LOOK LIKE THEY'D BE BINNED.
  is.binned = any(sapply(built$plot$layers, function(layer) {
    inherits(layer$stat, "StatBin") || inherits(layer$stat, "StatBin2d")
  }))

  #IF ANY ARE BINNED, SUPPRESS THE LIMITS
  breaks_limits = list(breaks = res$breaks, limits = res$limits)
  if(is.binned) { breaks_limits = list(breaks = res$breaks) }

  #IF THE USER HAS REQUESTED THINNED AXIS LABELS, WE SET THOSE TOO BY TAKING THE BREAKS AND REPLACING EVERY EVEN-INDEXED ONE WITH AN EMPTY STRING.
  if(object$thin_labels == TRUE) {
    tmp = res$breaks
    tmp[seq(from = 2, to = length(tmp), by = 2)] = ""
    breaks_limits$labels = tmp
  }

  plot + do.call(scale_y_continuous,
                      c(breaks_limits,
                        object$extra_args)) #CALL SCALE_X_CONTINUOUS AS NORMAL EXCEPT OVERRIDE IN THE NEW BREAKS AND LIMITS.
  } else {
    warning("The y scale is not numeric, so your scale_y_continuous_plus() command was ignored.")
    return(plot)
  }
}

#' Add A scale_fill_cont_plus-generated fill Scale Bar Gradation to a ggplot
#'
#' This method defines how objects of class `scale_fill_continuous_plus`, added by the function of the same name, are added to a ggplot2 plot using the `+` operator.
#' It ensures that the new, "pretty" breaks, now successfully anchored at or near the range values for the data in question, are added to the plot's x axis.
#'
#' @param object An object of class `scale_fill_cont_plus`, created by `scale_fill_continuous_plus()`, containing user-provided arguments (if any) or else pre-defined default values that find a set of "pretty" breaks that encompass the full range of values on the axis and that expand the limits of the axis slightly, if needed, to accomplish this.
#' @param plot A ggplot object to which the new fill color bar should be added.
#' @param name Internal name used by ggplot2 when adding the layer.
#'
#' @return A ggplot object with the fill scale breaks and limits redefined.
#' @export
ggplot_add.scale_fill_cont_plus = function(object, plot, name) {

  #CHECK TO SEE IF FILL WAS MAPPED IN GGPLOT GLOBALLY--THIS IS DIFFERENT THAN WITH X AND Y BECAUSE SOMETIMES THESE CAN BE PROCEDURALLY GENERATED BUT COLOR AND FILL CAN'T--THEY NEED TO BE EXPLICITLY MENTIONED SOMEHOW. ALSO, WE NEED TO GET THE *RAW* DATA FROM THE VARIABLE MAPPED TO FILL/COLOR, NOT THE FILL/COLOR COLUMN ITSELF CUZ THAT'LL STORE COLOR DATA.
  if (!is.null(plot$mapping$fill)) {
    fill_expr = rlang::as_name(plot$mapping$fill)
    if(!is.null(plot$data) && fill_expr %in% names(plot$data)) {
      fill_data = plot$data[[fill_expr]] #IF SO, GRAB THE FILL DATA
    }
  } else {
    #OTHERWISE, GO THRU THE LAYERS IN THE PLOT
    for(layer in plot$layers) {
      #REFERENCE LAYER-SPECIFIC DATA OR ELSE FALL BACK TO GLOBAL IF NONE.
      layer_data = if(!is.null(layer$data) &&
                       !inherits(layer$data, "waiver") &&
                       length(layer$data) > 0) { layer$data } else { plot$data }
      if(!is.null(layer$mapping$fill)) { #FIND WHO'S MAPPED TO FILL AND GRAB THOSE DATA.
        fill_expr = rlang::as_name(layer$mapping$fill)
        if(!is.null(layer_data) && fill_expr %in% names(layer_data)) {
          fill_data = layer_data[[fill_expr]]
        }
      }
    }

    #IF WE COME UP EMPTY, LET'S ABORT MISSION WITH A WARNING.
    if(is.null(fill_data)) {
      warning("scale_fill_continuous_plus() could not locate a fill variable.")
      return(plot)
    }
  }

  #GATE TO ENSURE THE VARIABLE IS ACTUALLY NUMERIC--OTHERWISE, RETURN AN APPROPRIATE WARNING AND ABORT.
  if(is.numeric(fill_data) || inherits(fill_data, "Date")) {

  res = cont_breaks_plus(fill_data,
                         n = object$n,
                         buffer_frac = object$buffer_frac) #CALL OUR NEW BREAKS FUNCTION

  #IF FILL WASN'T EXPLICITLY MAPPED BUT INSTEAD DERIVED, BUT A NAME WAS PROVIDED FOR IT BY THE USER, MAKE SURE THAT GETS IN THERE.
  if(!"fill" %in% names(plot$mapping)) {
    if(!is.null(object$extra_args$name)) {
      plot$labels$fill = object$extra_args$name #STUFF IT IN BY FORCE.
      object$extra_args$name = NULL #WIPE THIS OUT.
    }
  }

  #ON BINNED GRAPHS LIKE HISTOGRAMS, IT'S PROBABLY NOT A GREAT IDEA TO ENFORCE THE LIMITS BECAUSE IT'S HARD TO SET THOSE TO WORK FOR BINS...
  built = suppressMessages(ggplot2:::ggplot_build.ggplot(plot)) #FAKE BUILD THE PLOT.
  #SEE IF ANY OF THE LAYERS LOOK LIKE THEY'D BE BINNED.
  is.binned = any(sapply(built$plot$layers, function(layer) {
    inherits(layer$stat, "StatBin") || inherits(layer$stat, "StatBin2d")
  }))

  #IF ANY ARE BINNED, SUPPRESS THE LIMITS
  breaks_limits = list(breaks = res$breaks, limits = res$limits)
  if(is.binned) { breaks_limits = list(breaks = res$breaks) }

  #IF THE USER HAS REQUESTED THINNED AXIS LABELS, WE SET THOSE TOO BY TAKING THE BREAKS AND REPLACING EVERY EVEN-INDEXED ONE WITH AN EMPTY STRING.
  if(object$thin_labels == TRUE) {
    tmp = res$breaks
    tmp[seq(from = 2, to = length(tmp), by = 2)] = ""
    breaks_limits$labels = tmp
  }

  plot + do.call(scale_fill_continuous,
                 c(breaks_limits,
                   object$extra_args)) #CALL SCALE_X_CONTINUOUS AS NORMAL EXCEPT OVERRIDE IN THE NEW BREAKS AND LIMITS.
  } else {
    warning("The fill scale is not numeric, so your scale_fill_continuous_plus() command was ignored.")
    return(plot)
  }

}

#' Add A scale_color_cont_plus-generated Color Scale Bar Gradation to a ggplot
#'
#' This method defines how objects of class `scale_color_continuous_plus`, added by the function of the same name, are added to a ggplot2 plot using the `+` operator.
#' It ensures that the new, "pretty" breaks, now successfully anchored at or near the range values for the data in question, are added to the plot's color bar.
#'
#' @param object An object of class `scale_color_cont_plus`, created by `scale_color_continuous_plus()`, containing user-provided arguments (if any) or else pre-defined default values that find a set of "pretty" breaks that encompass the full range of values on the axis and that expand the limits of the axis slightly, if needed, to accomplish this.
#' @param plot A ggplot object to which the new color bar should be added.
#' @param name Internal name used by ggplot2 when adding the layer.
#'
#' @return A ggplot object with the color scale breaks and limits redefined.
#' @export
ggplot_add.scale_color_cont_plus = function(object, plot, name) {

  #CHECK TO SEE IF FILL WAS MAPPED IN GGPLOT GLOBALLY--THIS IS DIFFERENT THAN WITH X AND Y BECAUSE SOMETIMES THESE CAN BE PROCEDURALLY GENERATED BUT COLOR AND FILL CAN'T--THEY NEED TO BE EXPLICITLY MENTIONED SOMEHOW. ALSO, WE NEED TO GET THE *RAW* DATA FROM THE VARIABLE MAPPED TO FILL/COLOR, NOT THE FILL/COLOR COLUMN ITSELF CUZ THAT'LL STORE COLOR DATA.
  if (!is.null(plot$mapping$colour)) {
    colour_expr = rlang::as_name(plot$mapping$colour)
    if(!is.null(plot$data) && colour_exp %in% names(plot$data)) {
      colour_data = plot$data[[colour_expr]] #IF SO, GRAB THE FILL DATA
    }
  } else {
    #OTHERWISE, GO THRU THE LAYERS IN THE PLOT
    for(layer in plot$layers) {
      #REFERENCE LAYER-SPECIFIC DATA OR ELSE FALL BACK TO GLOBAL IF NONE.
      layer_data = if(!is.null(layer$data) &&
                      !inherits(layer$data, "waiver") &&
                      length(layer$data) > 0) { layer$data } else { plot$data }
      if(!is.null(layer$mapping$colour)) { #FIND WHO'S MAPPED TO FILL AND GRAB THOSE DATA.
        colour_expr = rlang::as_name(layer$mapping$colour)
        if(!is.null(layer_data) && colour_expr %in% names(layer_data)) {
          colour_data = layer_data[[colour_expr]]
        }
      }
    }

    #IF WE COME UP EMPTY, LET'S ABORT MISSION WITH A WARNING.
    if(is.null(colour_data)) {
      warning("scale_colour_continuous_plus() could not locate a colour variable.")
      return(plot)
    }
  }

  #GATE TO ENSURE THE VARIABLE IS ACTUALLY NUMERIC--OTHERWISE, RETURN AN APPROPRIATE WARNING AND ABORT.
  if(is.numeric(colour_data) || inherits(colour_data, "Date")) {

  res = cont_breaks_plus(colour_data,
                         n = object$n,
                         buffer_frac = object$buffer_frac) #CALL OUR NEW BREAKS FUNCTION

  #IF COLOR WASN'T EXPLICITLY MAPPED BUT INSTEAD DERIVED, BUT A NAME WAS PROVIDED FOR IT BY THE USER, MAKE SURE THAT GETS IN THERE.
  if(!"colour" %in% names(plot$mapping)) {
    if(!is.null(object$extra_args$name)) {
      plot$labels$colour = object$extra_args$name #STUFF IT IN BY FORCE.
      object$extra_args$name = NULL #WIPE THIS OUT.
    }
  }

  #ON BINNED GRAPHS LIKE HISTOGRAMS, IT'S PROBABLY NOT A GREAT IDEA TO ENFORCE THE LIMITS BECAUSE IT'S HARD TO SET THOSE TO WORK FOR BINS...
  built = suppressMessages(ggplot2:::ggplot_build.ggplot(plot)) #FAKE BUILD THE PLOT.
  #SEE IF ANY OF THE LAYERS LOOK LIKE THEY'D BE BINNED.
  is.binned = any(sapply(built$plot$layers, function(layer) {
    inherits(layer$stat, "StatBin") || inherits(layer$stat, "StatBin2d")
  }))

  #IF ANY ARE BINNED, SUPPRESS THE LIMITS
  breaks_limits = list(breaks = res$breaks, limits = res$limits)
  if(is.binned) { breaks_limits = list(breaks = res$breaks) }

  #IF THE USER HAS REQUESTED THINNED AXIS LABELS, WE SET THOSE TOO BY TAKING THE BREAKS AND REPLACING EVERY EVEN-INDEXED ONE WITH AN EMPTY STRING.
  if(object$thin_labels == TRUE) {
    tmp = res$breaks
    tmp[seq(from = 2, to = length(tmp), by = 2)] = ""
    breaks_limits$labels = tmp
  }

  plot + do.call(scale_colour_continuous,
                 c(breaks_limits,
                   object$extra_args)) #CALL SCALE_X_CONTINUOUS AS NORMAL EXCEPT OVERRIDE IN THE NEW BREAKS AND LIMITS.
  } else {
    warning("The color scale is not numeric, so your scale_color_continuous_plus() command was ignored.")
    return(plot)
  }

}

