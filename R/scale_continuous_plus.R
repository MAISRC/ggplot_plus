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

    #IF THERE IS, WE JUST RETURN THE BREAKS AND LIMITS AS SET BY THE DATA AND PRETTY_BREAKS
    if (has_low && has_high) {
      return(list(breaks = brks, limits = c(min(brks), max(brks))))
    }

    #IF NOT, WE ARTIFICIALLY DECREASE THE LOWER/UPPER VALUES BY ONE STEP AND THEN REPEAT THE PROCESS UNTIL WE SUCCEED.
    if (!has_low) lower = lower - step
    if (!has_high) upper = upper + step
  }

  #EITHER WAY, MAKE SURE WE COME OUT WITH BREAKS AND LIMITS EVEN IF THE ABOVE FAILS TO FIND A SOLUTION IN X TRIES.
  brks = breaks_fn(c(lower, upper))
  list(breaks = brks, limits = c(min(brks), max(brks)))
}


#' Find Pretty Breaks for a Continuous X axis in ggplot While Ensuring End Labels
#'
#' This function attempts to find a set of breaks for a continuous variable mapped to the x axis of a ggplot graph such that there aren't too many breaks, the breaks are "pretty" values where possible, and breaks exist at or near to the range values of the variable. It uses `cont_breaks_plus()` to do this--see there for more information.
#'
#' @param ... Standard inputs normally given to `scale_x_continuous()`. Must not include `breaks` or `limits` or an error will be returned, as the function attempts to circumvent the need to specify prettier breaks or appropriate limits.
#' @param n A length-1 numeric value for the "target" number of breaks to create. Defaults to 5. Passed to `cont_breaks_plus()` internally.
#' @param buffer_frac A length-1 numeric value corresponding to how close the end breaks must be to the end of the data for new breaks to not be added. Defaults to 0.05 (5%). A length-1 numeric value for the "target" number of breaks to create. Defaults to 5. Passed to `cont_breaks_plus()` internally.
#' @return Returns a list of class `"scale_x_cont_plus"`, which will trigger the `ggplot_add` method by the same name to trigger the axis breaks reconfiguration.
#' @examples
#' ggplot(iris, aes(x=Sepal.Length, y=Petal.Length)) + geom_plus(geom = "point") + scale_x_continuous_plus()
#' @export
scale_x_continuous_plus = function(...,
                                 n = 5,
                                 buffer_frac = 0.5) {
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
#' @return Returns a list of class `"scale_y_cont_plus"`, which will trigger the `ggplot_add` method by the same name to trigger the axis breaks reconfiguration.
#' @examples
#' ggplot(iris, aes(x=Sepal.Length, y=Petal.Length)) + geom_plus(geom = "point") + scale_y_continuous_plus()
#' @export
scale_y_continuous_plus = function(...,
                                 n = 5,
                                 buffer_frac = 0.5) {
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
                 extra_args = extra),
            class = "scale_y_cont_plus")
}

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
  x_data = plot$data[[rlang::as_name(plot$mapping$x)]] #GET THE X DATA
  res = cont_breaks_plus(x_data,
                       n = object$n,
                       buffer_frac = object$buffer_frac) #CALL OUR NEW BREAKS FUNCTION
  plot + do.call(scale_x_continuous,
          c(list(breaks = res$breaks, limits = res$limits),
            object$extra_args)) #CALL SCALE_X_CONTINUOUS AS NORMAL EXCEPT OVERRIDE IN THE NEW BREAKS AND LIMITS.
}

#' Add A scale_x_cont_plus-generated Y axis Gradation to a ggplot
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

  y_data = plot$data[[rlang::as_name(plot$mapping$y)]]
  res = cont_breaks_plus(y_data,
                       n = object$n,
                       buffer_frac = object$buffer_frac)
  plot + do.call(scale_y_continuous,
          c(list(breaks = res$breaks, limits = res$limits),
            object$extra_args))
}
