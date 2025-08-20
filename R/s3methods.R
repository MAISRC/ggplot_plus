#' Ensures our plot carries the ggplot.plus intent storage and class info into subsequence S3 methods.
#' @param plot A `ggplot` object.
#'
#' @return The same `ggplot` object, with `plot$ggplot_plus` ensured to exist
#'   and class `"ggplot_plus"` attached.
#'
#' @details
#' This does **not** build, modify, or draw the plot. It only prepares the
#' object so the unified `ggplot_build.ggplot_plus()` / `ggplot_gtable.ggplot_plus_built()`
#' methods can later read the recorded intents.
#'
#' @seealso [ggplot_build.ggplot_plus()], [ggplot_gtable.ggplot_plus_built()]
#'
#' @keywords internal
#' @noRd
.ensure_ggplot_plus = function(plot) {
  if(is.null(plot$ggplot_plus)) { plot$ggplot_plus = list() } # IF PLOT IS NOT ALREADY MARKED FOR GGPLOT_PLUS PATHWAY, MARK IT W/ CLASS AND CREATE STORAGE SPACE FOR INTENTS
  if(!inherits(plot, "ggplot_plus")) { class(plot) = c("ggplot_plus", class(plot)) }
  plot
}


#' Ggplot.Plus Build Method
#'
#' @description
#' Package-specific build method that runs **after** `ggplot2::ggplot_build()`
#' and before the gtable stage. It reads any intents stored under
#' `plot$ggplot_plus` (e.g., from `gridlines_plus()`) and applies them in a way
#'  that respects ggplot2's "last merge wins" semantics.
#'
#' Concretely, this method:
#' \itemize{
#'   \item Adds major gridlines only along continuous axes (and blanks all minor
#'         gridlines) by appending a theme element to `built$plot`.
#'   \item Adjusts legend key appearance (e.g., make alphas opaque in keys,
#'         ensure reasonable point/line sizes) by attaching guide overrides
#'         to `built$plot` \emph{without} triggering a rebuild.
#'   \item Emits a friendly warning when colour/fill are mapped to discrete
#'         variables with many levels (default threshold = 8).
#' }
#'
#' @param plot A `ggplot` object that carries the `"ggplot_plus"` class and a
#'   `plot$ggplot_plus` intent list (typically set by `ggplot_add()` methods)
#'
#' @return
#' A `ggplot_build` object (the same structure returned by
#' `ggplot2::ggplot_build()`), augmented with class `"ggplot_plus_built"`
#' to allow the package's gtable method to run next.
#'
#' @details
#' **Order of operations**
#'
#' 1. The `"ggplot_plus"` class is temporarily removed to avoid recursion, then
#'    `ggplot2::ggplot_build(plot)` is called exactly once.
#' 2. Intents are read from `plot$ggplot_plus`.
#' 3. Gridlines: the method inspects trained scales/panel parameters to decide
#'    which axes are continuous, then appends a `theme()` to `built$plot`
#'    setting `panel.grid.major.{x,y}` and blanking minor grids.
#' 4. Legend overrides: if needed, `guides()` is appended to `built$plot`
#'    with `guide_legend(override.aes = ...)` (and `guide_colourbar()` for
#'    continuous colour/fill).
#' 5. Discrete-level warning: for `colour`/`fill`, the trained scale is queried
#'    and a warning is issued when the number of discrete levels exceeds a
#'    threshold.
#'
#' **Why no rebuild?** By appending theme/guide intents to `built$plot` and
#' letting the gtable step realize them, we preserve ggplot2's usual layering
#' and keep performance predictable.
#'
#' @seealso
#'   [ggplot2::ggplot_build()], the package's gtable hook
#'   `ggplot_gtable.ggplot_plus_built()`, and the intent setters:
#'   [gridlines_plus()], [geom_plus(), [yaxis_title_plus()]].
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p = ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Petal.Length, colour = Species)) +
#'   geom_plus("point") +
#'   gridlines_plus()
#'
#' # Building triggers this method:
#' b = ggplot_build(p)                    # S3 dispatch to ggplot_build.ggplot_plus
#' }
#'
#' @export
ggplot_build.ggplot_plus = function(plot) {

  class(plot) = setdiff(class(plot), "ggplot_plus") #AVOID RECURSION BY REMOVING TRIGGERING CLASS.
  built = ggplot2::ggplot_build(plot) #BUILD THE PLOT ONCE, AS THIS METHOD MUST.

  intents = plot$ggplot_plus # CONVENIENCE OBJ

  ###GRIDLINES PLUS OPERATIONS--ENDPOINT: THEME ADD TO BUILT$PLOT, NO REBUILDING.
  if(!is.null(intents$grid)) {

    #DO A THOROUGH LOOK-THROUGH OF THE BUILT/TRAINED PLOT FOR EVIDENCE OF CONTINUOUS AXES.
    x_is_cont = .axis_is_continuous(built, "x")
    y_is_cont = .axis_is_continuous(built, "y")

    grid_intents = intents$grid #UNPACK GRID INTENTS

    #CONDITIONALLY ADD GRIDLINES
    grid_theme = ggplot2::theme(
      panel.grid.major.x = if(x_is_cont) {
        ggplot2::element_line(color = grid_intents$color,
                              linewidth = grid_intents$linewidth,
                              linetype = grid_intents$linetype)
      } else {
        ggplot2::element_blank()
        },
      panel.grid.major.y = if(y_is_cont) {
        ggplot2::element_line(color = grid_intents$color,
                              linewidth = grid_intents$linewidth,
                              linetype = grid_intents$linetype)
      } else {
        ggplot2::element_blank()
        },
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    )
    built$plot = built$plot + grid_theme #ADD THEM TO THE ALREADY BUILT PLOT SO THEY ARE DURABLE.

  } #END GRIDLINES_PLUS OPERATIONS GATE.


 ### GEOM_PLUS OPERATIONS

 ### #1 -- LEGEND KEY OVERRIDING CHECK
  if(!isFALSE(intents$guides$needs_override)) { #<--BY CHECKING LIKE THIS, WE NOW VIEW A NON-REFERENCE AS AN INTENT TO CHECK, SUCH AS VIA THE GEOM_POINT_PLUS PATHWAY, WHICH WOULDN'T SET IT AT ALL.

    #CATALOGUE ALL MAPPED AESTHETICS ANYWHERE (GLOBAL OR LOCAL)
    global_aes = names(built$plot$mapping)
    layer_aes  = unique(unlist(lapply(built$plot$layers, function(l) names(l$mapping))))
    present_aes = unique(c(global_aes, layer_aes))
    present_aes[present_aes == "color"] = "colour" #SPELLING NORMALIZATION
    aes_to_override = intersect(present_aes, c("colour","fill","shape","size", "alpha", "linetype", "linewidth")) #WILL BE A CHARACTER(0) HERE IF EMPTY.

    #DON'T OVERRIDE ANY SCALES THAT ARE SET TO NO GUIDE.
    aes_to_override = Filter(function(aes) {
      .should_consider_guide(built, aes) },
      aes_to_override)

    ##DO WE ACTUALLY NEED TO OVERRIDE? CHECK FOR SMALL SIZE/ALPHA/LINEWIDTHS
    needs_override = FALSE
    for(layer_data in built$data) {
      if(any(c("alpha","size", "linewidth") %in% names(layer_data))) {
        ua = unique(layer_data$alpha)
        us = unique(layer_data$size)
        ul = unique(layer_data$linewidth)
        if((length(ua) == 1 && is.finite(ua) && ua < 1) ||
           (length(us) == 1 && is.finite(us) && us < 5) ||
           (length(ul) == 1 && is.finite(ul) && ul < 1.2)) {
          needs_override = TRUE
          break
        }
      }
    }

    #ASSUMING A NEED TO OVERRIDE...
    if(needs_override && length(aes_to_override) > 0) {

    #PER AESTHETIC WITH A POSSIBLE GUIDE, BUILD A GUIDE_OVERRIDE OBJECT.
    overrides = lapply(aes_to_override, function(aes) {
      is_continuous = .aes_is_continuous(built, aes)
      if(aes %in% c("fill","colour") && is_continuous) {
        ggplot2::guide_colourbar() #WE'VE GOT A COLOR BAR, NOT A LEGEND.
      } else {

        #IF USERS ARE USING POINTS_PLUS, ENFORCE A SIZE OVERRIDE.
        uses_point_plus = any(vapply(built$plot$layers,
                                     function(l) inherits(l$geom, "PointPlus"), logical(1)))

        size_override = if (isTRUE(intents$guides$custom_shapes) || uses_point_plus) 8 else 5

        #WE HAVE A LEGEND--OVERRIDE AES
        first_pass = list( #LIST OF OVERRIDES
          alpha = 1,
          size = size_override,
          linewidth = 1.2
        )
        #REMOVE SOME OVERRIDES IF WE'RE LOOKING AT A SCALE FOR THAT AND VARIANCE IS INTENDED.
        if(aes == "alpha") {
          first_pass$alpha = NULL
        }
        if(aes == "size") {
          first_pass$size = NULL
        }
        if(aes == "linewidth") {
          first_pass$linewidth = NULL
        }
        ggplot2::guide_legend(
          override.aes = first_pass)
      }
    })
    names(overrides) = aes_to_override #WILL BE A NAMED EMPTY LIST HERE IF EMPTY

    new_plot = built$plot + do.call(ggplot2::guides, overrides) #APPLY NEW GUIDES TO A NEW PLOT OBJ
    class(new_plot) = setdiff(class(new_plot), "ggplot_plus") #TEMPORARILY REMOVE CLASS TO PREVENT RECURSION
    rebuilt = suppressWarnings(ggplot2::ggplot_build(new_plot)) #REBUILD THIS GRAPH HOWEVER MANY TIMES NEEDED TO SYNC UP THE NEW GUIDES WITH THE KEYS/PARAMS
    rebuilt$plot$ggplot_plus = built$plot$ggplot_plus #PORT OVER THE OLD INTENTS INFO
    built = rebuilt #OVERRIDE NOW TO PRESERVE BUILT AS REFERENCED OBJECT FROM HERE ON.
    intents = built$plot$ggplot_plus #RE-ESTABLISH INTENTS FOR DOWNSTREAM CODE JUST IN CASE.

    }
  }


  ### #2 -- USER GUIDANCE WARNINGS
  if(!isTRUE(intents$warnings$silence)) {

    #2A -- NOT LABELING ONE'S SCALES

      violations = list() #STORAGE OBJ FOR "VIOLATORS"

      #NOT ALL AES ARE TITLE-ABLE, SO WE FILTER HERE.
      candidates = intersect(
        present_aes,
        c("x","y","colour","fill","shape","size","linetype","alpha","linewidth")
      )

      for(aes in candidates) { #GO THRU THESE AES.****WOULDN'T WORK IF PRESENT_AES WERE EVER OUT OF SCOPE.

        #SHOULD WE POTENTIALLY WARN FOR THIS AES? DEFAULT TO YES FOR POSITIONAL, NO IF THEY'VE TURNED THE GUIDE OFF.
        should_warn = FALSE
        if(aes %in% c("x","y")) { should_warn = TRUE }
        sc = built$plot$scales$get_scales(aes)
        if(is.null(sc)) { #NOT ENOUGH INFO TO SEE IF GUIDE IS OFF, SO STAY TO YES
          should_warn = TRUE
        } else {
          should_warn = is.null(sc$guide) || #YES, GUIDE IS OFF, DON'T WARN.
          !identical(sc$guide, "none")
        }

        if(should_warn == FALSE) { next } #SKIP AT THIS POINT

        #NOW, TRY TO GET THE VARIABLE NAMES FOR THIS AES
        mapped_vars = .ggplus_mapped_vars_for_aes(built$plot, aes)

        if(length(mapped_vars) == 0) { next } #SKIP IF NONE ARE FOUND

        #NOW, WE FIND WHAT THE FINAL TITLE OF THE SCALE WAS FOR COMPARISON
        if(!is.null(sc) &&
            !inherits(sc$name, "waiver") &&
            nzchar(sc$name)) {
          final_title = sc$name #TRAINED SCALE NAME FIRST
        } else {

          lab = built$plot$labels[[aes]]
          if (!is.null(lab) &&
              !inherits(lab, "waiver") &&
              nzchar(lab)) {
            final_title = lab #OTHERWISE, PLOTS$LABELS
          } else {
            final_title = NA_character_
          }

        }

        title_clean = trimws(gsub("`", "", as.character(final_title)))
        inside = sub(".*\\(([^()]+)\\).*", "\\1", title_clean)
        inside = trimws(strsplit(inside, ",", fixed = TRUE)[[1]][1])
        if(nzchar(inside)) {title_clean = inside}

        #NOW, WE HAVE TO COMPARE EVERY MAPPED VARIABLE NAME TO THE TITLE
        offenders = mapped_vars[
          vapply(mapped_vars, function(v) {
            v_clean = trimws(gsub("`", "", as.character(v)))

            #RETAIN ONLY THE MATCHES.
            identical(v_clean, title_clean)
          }, logical(1))
        ]

        #IF THERE WAS NO TITLE AT ALL, WARN:
        if(is.na(title_clean) ||
           !nzchar(title_clean)) {
          offenders = unique(c(offenders, mapped_vars))
        }

        #IF ANY VIOLATIONS DETECTED, STORE:
        if(length(offenders) > 0) {
          violations[[aes]] = unique(offenders)
        }
       }

      #ONE WARNING IF ANY VIOLATIONS
      if(length(violations)) {
        msgs = vapply(names(violations), function(aes) {
          v = paste(violations[[aes]], collapse = ", ")
          sprintf("%s: %s", aes, v)
        }, character(1))
        warning(
          paste0(
            "Some mapped aesthetics appear to be using default titles. Consider setting human-readable titles (with units) via scale_*() or labs().\n",
            "Unrenamed: ", paste(msgs, collapse = ", "),
            ". Set silence_warnings = TRUE in geom_plus() to suppress warnings like these."
          ),
          call. = FALSE
        )
      }

    #2B -- TOO MANY DISCRETE LEVELS FOR COLOR

    for(aes in c("colour", "fill")) {

        #SKIP IF THIS SCALE IS CONTINUOUS
      if(.aes_is_continuous(built, aes)) { next }

      n_levels = .aes_num_levels(built, aes) #GET NUM LEVELS OTHERWISE (OR TRY)
      if(is.na(n_levels)) { next }

      max_ok = 8

      if(is.finite(n_levels) &&
         n_levels > max_ok) {

        #TRY TO GET THE NAME OF THE VARIABLE HERE.
        pretty_name =
          tryCatch({
            sc = built$plot$scales$get_scales(aes)
            nm = if(!is.null(sc) &&
                    !inherits(sc$name, "waiver")) {
              sc$name
            } else { NULL }

            if(is.null(nm)) { built$plot$labels[[aes]] } else { nm }
          }, error = function(e) NULL)

        lab = if(is.null(pretty_name) || !nzchar(pretty_name)) {
          sprintf("The variable mapped to %s", aes)
        } else {
          paste0("\"", pretty_name, "\"")
        }

        warning(sprintf(
          "%s has %d discrete levels. Beyond ~%d categories, colours become hard to distinguish. Consider using another channel (e.g., shape), multiple intersecting channels (e.g. fill color and pattern), or reducing the number of groups. Set silence_warnings=TRUE in geom_plus() to hide messages like this.",
          lab, n_levels, max_ok
        ), call. = FALSE)
      }
    }
  }

  ### END OPERATIONS HERE ###

  class(built) = c("ggplot_plus_built", class(built)) #ADD NEXT CLASS TO TRIGGER GTABLE METHOD
  built #RETURN.

}





#' Gtable Method For Ggplot.Plus
#'
#' This S3 method runs after the build step and before drawing. If the user
#' called `yaxis_title_plus()` (i.e., y-axis intents are present), it:
#'
#' * inserts a brand-new row above the top panel (or below the bottom panel)
#'   and renders the y-axis title horizontally there (or the x-axis title when
#'   `coord_flip()` is used),
#' * styles the title using the plot’s theme (`axis.title.y` / `axis.title.x`)
#'   via `calc_element()`, and
#' * optionally moves any top legends down into the new title row, collapsing
#'   any now-empty spacer rows.
#'
#' Default x/y title grobs are deleted to avoid double-rendering.
#'
#' @param data A built plot object (the return value of
#'   `ggplot2::ggplot_build()`), carrying class `"ggplot_plus_built"`.
#'
#' @return A `gtable` ready to draw.
#'
#' @details
#' **Title text source.** We prefer the trained scale’s `name`. If that’s not
#' set, we fall back to `plot$labels[[axis]]`. As a last resort, we use a
#' placeholder to nudge users toward a `scale_*()` or `labs()` call.
#'
#' **Row/column targeting.** We find the panel band rows and the left axis
#' column dynamically from the gtable layout (no hard-coded row numbers),
#' then insert a new row and place the title in the leftmost axis column. Using
#' a new row ensures no easy collisions with existing plot elements.
#'
#' **coord_flip().** When `coord_flip()` is active, we treat the *x* title as
#' the “y title” to move, so the horizontal label still lands in the correct,
#' logical place for the flipped orientation.
#'
#' @examples
#' library(ggplot2)
#' ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   geom_plus("point") +
#'   yaxis_title_plus("top")                 # move y title to a new top row
#'
#' ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = factor(cyl))) +
#'   geom_plus("point") +
#'   theme_plus(legend_pos = "top") +
#'   yaxis_title_plus("top", move_top_legend_down = TRUE)
#'
#' @importFrom ggplot2 ggplot_gtable calc_element
#' @export
ggplot_gtable.ggplot_plus_built = function(data) {

  class(data) = setdiff(class(data), "ggplot_plus_built") #AVOID RECURSION BY REMOVING TRIGGERING CLASS.

  intents = data$plot$ggplot_plus$yaxis #UNPACK INTENTS

  gt = ggplot2::ggplot_gtable(data) #ASSEMBLE GTABLE, AS THIS METHOD MUST.

  if(is.null(intents)) { #IF THERE ARE NO YAXIS INTENTS, END.
    return(gt)
  }

  ##STOP TO CONSIDER coord_flip TO DETERMINE WHICH AXIS WE'RE RETITLING.
  real_scale = if(inherits(data$plot$coordinates, "CoordFlip")) { "x" } else { "y" }
  title_element_name = if(real_scale == "y") { "axis.title.y" } else { "axis.title.x" }

  #UNPACK USER INTENTS
  location = intents$location %||% "top"
  move_top_legend_down = isTRUE(intents$move_top_legend_down)

  #FIGURE OUT WHAT THE TITLE LABEL SHOULD BE.
  sc = data$plot$scales$get_scales(real_scale) #PREFER THE TRAINED SCALE NAMES.
  if(!is.null(sc) &&
     !inherits(sc$name, "waiver") &&
     nzchar(sc$name)) {
    lab = sc$name
  } else {
    lab = data$plot$labels[[real_scale]] #ELSE GRAB FROM THE LABELS LIST.
    if(is.null(lab) ||
       inherits(lab, "waiver") ||
       !nzchar(lab)) {
      lab = "Placeholder. Replace w/ labs(y = ...)."
    }
  }

  #THEN, KILL THE EXISTING TITLE GROB IN THE GTABLE SO IT DOESN'T ALSO APPEAR.
  kill_names = if(real_scale == "y") {
    c("ylab-l", "ylab-r")
  } else {
    c("xlab-t", "xlab-b")
  }
  kill_idx = which(gt$layout$name %in% kill_names)
  if(length(kill_idx) > 0) { #OVERWRITE THEM WITH ZEROGROBS.
    gt$grobs[kill_idx] = replicate(length(kill_idx),
                                   ggplot2::zeroGrob(),
                                   simplify = FALSE)
  }

  ##THEN, WE TRY TO FIND THE PANEL ROW AND LEFT-AXIS LABELS COLUMNS DYNAMICALLY...
  panel_rows = which(grepl("^panel", gt$layout$name))
  if(!length(panel_rows)) {
    return(gt) #IF SOMEHOW NO PANELS, BREAK.
  }

  top_panel_t = min(gt$layout$t[panel_rows]) #TOP SUCH ROW
  bottom_panel_b = max(gt$layout$b[panel_rows]) #BOTTOM SUCH ROW

  #COLUMN SHOW BE THE LEFT-HAND SIZE OF THE AXIS-LABELS ROW OR THE PANEL ROWS.
  axis_l_rows = which(grepl("^axis-l", gt$layout$name)) #USUALLY, AXIS INFO.
  left_candidates = axis_l_rows

  title_col = if(length(left_candidates) > 0) {
     min(gt$layout$l[left_candidates]) } #GET THE MINIMUM ONE, IF THERE ARE MANY.
    else {
     min(gt$layout$l[panel_rows]) #SAME HERE.
    }

  #THEN, INSERT ENTIRELY NEW ROW FOR THE TITLE, EITHER ON TOP OR ON BOTTON, JUST BEYOND PANEL ROWS.
  insert_pos = if(location == "top") {
    top_panel_t - 1L
  } else {
    bottom_panel_b
  }
  gt = gtable::gtable_add_rows(gt, heights = grid::unit(1.5, "lines"), pos = insert_pos)
  title_row = insert_pos + 1 #THE INDEX OF THAT ROW WILL BE 1 HIGHER THAN WHERE WE INSERTED IT.

  #THEN, BEGIN BUILDING THE TEXT GROB. SHOULD USE THE THEME STYLES FROM THE PREVIOUS TITLE.
  el = ggplot2::calc_element(title_element_name, data$plot$theme)
  gp = .ggplus_element_to_gpar(el) #JUST TRANSLATES THEME ARG NAMES TO GPAR ARG NAMES.
  vjust_val = if(location == "top") { 0.25 } else { 0.75 } #SOME OPINIONATED VJUST VALUES HERE...****

  #ACTUALLY ADD THE GROB
  gt = gtable::gtable_add_grob(
    gt,
    grob = grid::textGrob(
      lab,
      x = 0, y = 0.5,
      hjust = 0, vjust = vjust_val,
      rot = 0,
      gp = gp
    ),
    t = title_row, l = title_col,
    name = paste0("ggplotplus-", real_scale, "-title"),
    clip = "off"
  )

  ##LASTLY, IF USER HAS REQUESTED MOVING A TOP LEGEND STRIP DOWN INTO TITLE ROW, DO SO NOW.
  # 9) Optionally move a top legend down into the same new row (if present)
  if(move_top_legend_down) {
    guide_rows = which(grepl("^guide-box", gt$layout$name))

    if(length(guide_rows)) {

      is_top = gt$layout$t[guide_rows] < top_panel_t
      top_guides = guide_rows[is_top]

      if(length(top_guides)) {

        #HAVING FOUND A TOP GUIDE, RELOCATE THEM TO TITLE_ROW
        prev_top = min(gt$layout$t[top_guides])
        prev_bottom = max(gt$layout$b[top_guides]) #STASH PREV ROWS

        gt$layout$t[top_guides] = title_row #MOVE TO NEW ROWS.
        gt$layout$b[top_guides] = title_row

        #REDUCE THE ROWS CONTAINING THE TOP LEGENDS BEFORE AND NOTHING ELSE TO 0 HEIGHTS
        for(r in prev_top:prev_bottom) {
          occupants = which(gt$layout$t <= r &
                              gt$layout$b >= r)
          names_here = gt$layout$name[occupants]
          only_spacers = all(is.na(names_here)) ||
            all(names_here %in% c("spacer", "background"))
          if(only_spacers) {
            gt$heights[r] = grid::unit(0, "cm")
          }
        }

      }
    }
  }

  gt #RETURN
}
