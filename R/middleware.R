#' An Opinionated Version of `ggplot2`'s Default `theme_gray` Plot Theme
#'
#' A set of replacement arguments to `ggplot2`'s default `theme_gray()` plot theme, affecting elements such as axis lines and text, legend lines and rectangles, gridlines and plot backgrounds, and strip text and placement.
#'
#' @return A object of class "theme_plus," which will subsequently trigger the `ggplot_add.theme_plus` S3 method.
#' @export
default_theme = theme_gray() %+replace% #<--SPECIAL GGPLOT2 OPERATOR FOR THEMES
  ggplot2::theme(
    # aspect.ratio = 1, #I WANT TO DO MORE RESEARCH ON WHAT THIS VALUE SHOULD BE OR HOW TO CALCULATE IT.
    axis.line = ggplot2::element_line(color = "black", linewidth = 1.2, lineend = "square"), #ADD THICK BLACK X AND Y AXIS LINES WITH SQUARE ENDS TO ENSURE THAT THEY APPEAR TO VISUALLY MEET.
    axis.title.x = ggplot2::element_text(color = "black", size = 18, margin = ggplot2::margin(t = 10)), #ADD TOP MARGIN TO X AXIS TITLE.
    axis.title.y = ggplot2::element_text(color = "black", size = 18, vjust = 0.5, margin = ggplot2::margin(r  = 15), angle = 90),
    axis.text = ggplot2::element_text(size = 16, color = "black"), #ENSURE AXIS LABELS ARE BLACK AND SIZE 16
    axis.ticks.length = ggplot2::unit(0.3, "cm"), #INCREASE SIZE OF AXIS TICK MARKS TO BE MORE NOTICEABLE.
    legend.title = ggplot2::element_text(color = "black", size = 18),
    legend.text = ggplot2::element_text(size = 16, color = "black"),
    legend.key = ggplot2::element_rect(fill = "transparent", color = "white"),
    legend.background = ggplot2::element_rect(color = NA, fill = "#FFFEFD"),
    legend.ticks.length = ggplot2::unit(0.2, "cm"),
    legend.frame = ggplot2::element_rect(color = "black", linewidth = 1.2), #MAKE SOLID BLACK LINES FOR THE LEGEND BORDER FOR CONTINUOUS SCALES.
    legend.ticks = ggplot2::element_line(color = "white", linewidth = 1.2, linetype = "solid"), #MAKE THE TICKS WHITE
    panel.border = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(), #ELIMINATE MAJOR AND MINOR GRIDLINES
    panel.background = ggplot2::element_rect(fill = "#FFFEFD", color = NA), #SWITCH FROM GRAY TO WHITE BACKGROUND
    panel.spacing = ggplot2::unit(1, "cm"),
    plot.background = ggplot2::element_rect(fill = "#FFFEFD", color = NA),
    plot.title = ggplot2::element_blank(),
    plot.subtitle = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(color = "white", fill = "white"),
    strip.text = ggplot2::element_text(color = "black", size = 16, face = "bold"),
    strip.text.y = element_text(margin = margin(l=5), angle = 0),
    strip.text.x = element_text(margin = margin(b=5)),
    strip.placement = "outside", #THIS ALWAYS ENSURES THAT AXIS LABELS GO CLOSER TO THE AXIS THAN THE STRIP LABELS WOULD.
    complete = TRUE #<--KEY INPUT, ENSURES FEWER SURPRISES IN HOW THIS THEME BEHAVES VIS-A-VIS A DEFAULT THEME. COMPLETE THEMES ARE TREATED AS A COLLECTION OF FALLBACK VALUES FOR WHEN A USER DOESN'T SPECIFY SOMETHING OR DOESN'T INHERIT SOMETHING FROM A MORE GLOBAL VALUE.
  )

#' Choose Legend-Position-Dependent Theme Arguments For `theme_plus()`
#'
#' This helper decides which block of `ggplot2::theme()` settings to
#' apply inside `theme_plus()` based on the requested legend
#' position. Only positions of `"top"` or `"right"` are currently supported.
#' This function is not meant to be called explicitly by the user.
#'
#' @param legend_pos Either `NULL` (default) or a length-1 string.
#'   Currently recognized values are `"top"` and `"right"`. Any other
#'   values default to `"right"`.
#'
#' @return A `ggplot2::theme` object containing only legend-related
#'   settings (key size, spacing, justification, etc.).
#'
#' @export
determine_legend_theme = function(legend_pos) {
if(!is.null(legend_pos) &&
   legend_pos == "top") {
  theme2add = ggplot2::theme(
    legend.key.width = ggplot2::unit(1.5, "cm"),
    #GREATLY EXPAND THE WIDTH
    legend.key.height = ggplot2::unit(0.75, "cm"),
    #EXPAND THE HEIGHT A BIT ALSO.)
    legend.title = ggplot2::element_text(margin = ggplot2::margin(r = 15), vjust = 0.5),
    legend.box.just = "bottom",
    legend.justification = "right",
    legend.key.spacing.x = ggplot2::unit(0.5, "cm"),
    legend.position = "top",
    legend.direction = "horizontal"
  )
} else {
  theme2add = ggplot2::theme(legend.key.height = ggplot2::unit(1.5, "cm"),
                             legend.key.width = ggplot2::unit(0.75, "cm"),
                             legend.title = ggplot2::element_text(margin = ggplot2::margin(b = 15), hjust = 0.5),
                             legend.box.just = "right",
                             legend.justification = "right",
                             legend.key.spacing.y = ggplot2::unit(0.5, "cm"),
                             legend.position = "right",
                             legend.direction = "vertical")
}
return(theme2add)
}

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

