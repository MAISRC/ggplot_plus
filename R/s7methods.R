# METHODS DISPATCHING -----------------------------------------------------
#' Ensures our plot carries the ggplotplus intent storage and class info into subsequence S7 dispatch methods.
#' @param plot A `ggplot` object.
#'
#' @return The same `ggplot` object, with `plot@ggplotplus` ensured to exist.
#'
#' @details
#' This does **not** build, modify, or draw the plot. It only prepares the
#' object so the custom S7 methods of ggplotplus can
#' later read the recorded intents during dispatch.
#'
#' @keywords internal
#' @noRd
.ensure_ggplotplus_plot = function(plot) {

  if(!inherits(plot, "GGPlotPlusPlot")) {
    plot = S7::convert(plot, GGPlotPlusPlot)
  }

  if(is.null(plot@ggplotplus)) {
    plot@ggplotplus = GGPlotPlusState()
  }

  plot
}

# S7 UPDATE_GGPLOT METHODS ------------------------------------------------
# update_ggplot = ggplot2::update_ggplot #JUST TO GET AROUND THE PARSER FOR TESTING!****

#' Add gridlines_plus() intent to a ggplot object
#'
#' Internal S7 method for adding `GridlinesPlus` objects to ggplot2 plots.
#' Ensures the plot carries ggplotplus state, then stores the gridline intent
#' for resolution during plot building after scales have been trained.
#'
#' @keywords internal
#' @noRd
S7::method(update_ggplot, #REGISTER A NEW SPECIFIC VERSION OF THE GENERIC UPDATE_GGPLOT2 METHOD
           list(GridlinesPlus, ggplot2::class_ggplot)) <- function(object, plot, ...) { #NOTE THAT THE SYNTAX HERE WITH THE ARROW OPERATOR IS ESSENTIAL
    plot = .ensure_ggplotplus_plot(plot) #ALWAYS RUN FIRST TO MAKE SURE GGPlotPlus_State exists
    plot@ggplotplus@grid = object #THE OBJECT IS THE INCOMING BITS AND BOBS FROM gridlines_plus. SINCE ITS A CLEARLY DEFINED S7 CLASS OBJECT ALREADY, WE USE @ TO REFER TO IT AND ALSO HAVE NO NEED TO UNPACK IT HERE.
    return(plot)
}


#' Add yaxis_title_plus() intent to a ggplot object
#'
#' Internal S7 method for adding `YAxisTitlePlus` objects to ggplot2 plots.
#' Ensures the plot carries ggplotplus state, applies any margin or legend
#' nudging needed before layout, and stores the y-axis title intent for
#' resolution during gtable construction.
#'
#' @keywords internal
#' @noRd
S7::method(update_ggplot,
           list(YAxisTitlePlus, ggplot2::class_ggplot)) <- function(object, plot, ...) {

  plot = .ensure_ggplotplus_plot(plot)

  #IF BUNDLING THE NUDGE FUNCTION, IMPLEMENT HERE.
  if(isTRUE(object@nudgeTopLegendDown)) {
    plot = plot + .nudge_top_legend_down(howMuch = object@nudgeHowMuch)
  }

  plot@ggplotplus@y_axis_title = object
  return(plot)
}


#' Add theme_plus() to a ggplot object
#'
#' Internal S7 method for adding `ThemePlus` objects to ggplot2 plots. Ensures
#' the plot carries ggplotplus state, applies the completed ggplot2 theme, and
#' stores theme-related intent for later geom-default adjustments during plot-
#' building.
#'
#' @keywords internal
#' @noRd
S7::method(update_ggplot,
           list(ThemePlus, ggplot2::class_ggplot)) <- function(object, plot, ...) {

             plot = .ensure_ggplotplus_plot(plot)

             plot = plot + object@theme2add #<-APPLY THE DEFAULT THEME.

             plot@ggplotplus@theme = object

             return(plot)
}

# S7 BUILD_GGPLOT METHOD --------------------------------------------------
# ggplot_build = ggplot2::ggplot_build #SAME****

#' Build a ggplotplus plot
#'
#' Internal S7 method for building `GGPlotPlusPlot` objects. Applies deferred
#' ggplotplus build-stage behavior before delegating to ggplot2's ordinary plot
#' build machinery.
#'
#' This method currently patches theme-plus geom defaults into layers before
#' building, applies scale-aware gridline theme adjustments after scales are
#' trained, and forwards any gtable-stage y-axis title intent by converting the
#' built object to `GGPlotPlusBuilt`.
#'
#' @keywords internal
#' @noRd
S7::method(ggplot_build, GGPlotPlusPlot) <- function(plot, ...) {

   ###THEME PLUS GEOM DEFAULTS OPERATIONS
  if(.s7_prop_is_true(plot@ggplotplus@theme, "applyGeomDefaults")) {

    plotLayers = plot$layers #ID ALL LAYERS (GEOM) IN THIS PLOT...
    if(length(plotLayers) > 0) { #POSSIBLE THEY PUT IN NO GEOMS, IN WHICH CASE THERE'D BE NO LAYERS.
    for(layer in 1:length(plotLayers)) { #GO LAYER BY LAYER

    thisLayer = plotLayers[[layer]] #ISOLATE THIS LAYER
    geom2match = class(thisLayer$geom)[1] #GET THE CLASS TYPE OF THIS GEOM
    geom2match = tolower(sub("^Geom", "", geom2match)) #YANK OUT THE GEOM BIT AND COERCE TO LOWER.

    defaults2use = geom_plus_defaults[[geom2match]] #GRAB THE DEFAULTS FOR IT FROM OUR GLOBAL OBJ.
    if(is.null(defaults2use)) { next } #SKIP IF NONE.

    if(length(defaults2use$aes) > 0) { #IF THIS GEOM HAS AES DEFAULTS...
    for(aes in 1:length(defaults2use$aes)) { #GO ONE BY ONE THRU THEM

      currAes2Check = names(defaults2use$aes[aes]) #GET NAME OF CURR AES

      if(any( #IF THE USER HAS ALREADY SPECIFIED SOMETHING FOR THIS AES, SKIP.
        currAes2Check %in% names(thisLayer$mapping),
        (currAes2Check %in% names(plot$mapping) && isTRUE(thisLayer$inherit.aes)),
        currAes2Check %in% names(thisLayer$aes_params)
      )) { next }

      #OTHERWISE, INSERT DEFAULT INTO OBJ.
      plot$layers[[layer]]$aes_params[[currAes2Check]] = defaults2use$aes[[aes]]

      }#END AES DEFAULTS LOOP

    } #END AES DEFAULTS REGION

    if(length(defaults2use$params$geom_params) > 0) {
      for(Gparam in 1:length(defaults2use$params$geom_params)) {

        currParam2Check = names(defaults2use$params$geom_params[Gparam])

        if(.param_is_already_set(thisLayer, currParam2Check)) {
          next
        }

        #OTHERWISE, INSERT DEFAULT INTO OBJ.
        plot$layers[[layer]]$geom_params[[currParam2Check]] = defaults2use$params$geom_params[[Gparam]]
      }
    }
    if(length(defaults2use$params$stat_params) > 0) {
      for(Sparam in 1:length(defaults2use$params$stat_params)) {

        currParam2Check = names(defaults2use$params$stat_params[Sparam])

        if(any( #FEWER PLACES TO LOOK FOR LAYER PARAMETERS...
          currParam2Check %in% names(thisLayer$stat_params)
        )) { next }

        #OTHERWISE, INSERT DEFAULT INTO OBJ.
        plot$layers[[layer]]$stat_params[[currParam2Check]] = defaults2use$params$stat_params[[Sparam]]

        }

      }#END PARAM DEFAULTS LOOP

    } #END LAYER BY LAYER GEOM DEFAULTS CHECK
   } #NO LAYERS/NO LAYERS CHECK

  } #END THEME PLUS GEOM DEFAULTS REGION


    plain_plot = S7::convert(plot, ggplot2::class_ggplot) #CONVERT TO REGULAR OLD class_ggplot TO BUILD GENERICALLY.
    built = ggplot2::ggplot_build(plain_plot, ...)

    ###GRIDLINES PLUS OPERATIONS
    grid_intents = plot@ggplotplus@grid # CONVENIENCE OBJ
    if(!is.null(grid_intents)) {

      grid_theme = .apply_gridlines_plus(built, grid_intents) #<--GO SEE MIDDLEWARE.R FOR THIS HELPER.

      built$plot = built$plot + grid_theme

    } #END GRIDLINES_PLUS OPERATIONS


    #REGISTERING INTENT TO MOVE ALONG TO THE GTABLE METHOD FOR YAXIS TITLE PLUS AS NEEDED.
    if(!is.null(plot@ggplotplus@y_axis_title)) {
      built = S7::convert(built, GGPlotPlusBuilt)
      built@y_axis_title = YAxisTitlePlus(
          location = plot@ggplotplus@y_axis_title@location
        )
    }

    return(built)

}



# S7 GTABLE METHOD --------------------------------------------------------

# ggplot_gtable = ggplot2::ggplot_gtable #SAME****


#' Convert a ggplotplus built plot to a gtable
#'
#' Internal S7 method for converting `GGPlotPlusBuilt` objects to gtables.
#' Delegates first to ggplot2's ordinary gtable construction, then applies any
#' deferred ggplotplus layout edits that require access to the completed gtable.
#'
#' This method currently supports `yaxis_title_plus()`, which moves the y-axis
#' title into a custom row above or below the panel region.
#'
#' @keywords internal
#' @noRd
S7::method(ggplot_gtable, GGPlotPlusBuilt) <- function(data, ...) {

  plain_data = S7::convert(data, ggplot2::class_ggplot_built) #CONVERT TO REGULAR OLD class_ggplot TO GTABLE GENERICALLY.
  gtable = ggplot2::ggplot_gtable(plain_data)

  yaxis_intents = data@y_axis_title # CONVENIENCE OBJ

  ###Y-AXIS TITLE PLUS OPERATIONS
  if(is.null(yaxis_intents)) {
  return(gtable)
  }

  gtable = .apply_yaxis_title_plus(data, gtable, yaxis_intents)

  return(gtable)

}
