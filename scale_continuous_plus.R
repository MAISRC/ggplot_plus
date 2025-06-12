library(scales) #THIS IS WHERE PRETTY_BREAKS IS

cont_breaks_plus = function(data, 
                                   n = 5,
                                   buffer_frac = 0.05, 
                                   max_iter = 50) {
  
  breaks_fn = scales::pretty_breaks(n = n) #CREATE A BASE BREAKS FUNCTION AIMING FOR A SPECIFIC NUMBER OF BREAKS, BY DEFAULT 5.
  
  data_range = range(data, na.rm = TRUE) #THE ORIGINAL RANGE OF THE DATA
  original_lower = data_range[1] #THE ORIGINAL UPPER AND LOWER VALUES
  original_upper = data_range[2]
  lower = original_lower #THE CHANGING UPPER AND LOWER VALUES, AS WE NEED TO EXPAND THE LIMITS TO GET LABELS BY THE EDGES OF THE GRAPH
  upper = original_upper
  span = diff(data_range) #THE LENGTH OF THE RANGE BTW THESE VALUES.
  
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


#THIS FUNCTION IS A CONVENIENCE FUNCTION THAT SETS THE TARGET NUMBER OF BREAKS AND THE END-OF-AXIS BUFFER FRACTIONS AS WELL AS ANY OTHER ARGUMENTS AND PASSES THEM INTO A LIST THAT BEARS THE scale_x_cont_up CLASS. 
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

#SAME FOR THE Y AXIS.
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

#THESE FUNCTIONS ADD THESE TWO FUNCTIONS ABOVE TO THE GGPLOT METHODS SO THAT THEY CAN BE CALLED IN A USUAL GGPLOT + STRING.
ggplot_add.scale_x_cont_plus = function(object, plot, name) {
  x_data = plot$data[[rlang::as_name(plot$mapping$x)]] #GET THE X DATA
  res = cont_breaks_plus(x_data, 
                       n = object$n, 
                       buffer_frac = object$buffer_frac) #CALL OUR NEW BREAKS FUNCTION
  plot + do.call(scale_x_continuous, 
          c(list(breaks = res$breaks, limits = res$limits),
            object$extra_args)) #CALL SCALE_X_CONTINUOUS AS NORMAL EXCEPT OVERRIDE IN THE NEW BREAKS AND LIMITS.
}

ggplot_add.scale_y_cont_plus = function(object, plot, name) {
  
  y_data = plot$data[[rlang::as_name(plot$mapping$y)]]
  res = cont_breaks_plus(y_data, 
                       n = object$n, 
                       buffer_frac = object$buffer_frac)
  plot + do.call(scale_y_continuous, 
          c(list(breaks = res$breaks, limits = res$limits),
            object$extra_args))
}
