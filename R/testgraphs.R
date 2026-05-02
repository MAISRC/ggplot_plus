# library(ggplot2)
# library(ggplot_plus)

test_plots = list()

# 1. Basic scatter: point_plus + continuous x/y + y title + gridlines
test_plots$scatter_basic =
  ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
  geom_point_plus() +
  geom_smooth(method = "loess", formula = y ~ x) +
  theme_plus() +
  gridlines_plus() +
  yaxis_title_plus() +
  scale_continuous_plus(scale = "x") +
  scale_continuous_plus(scale = "y", name = "Sepal length")

# 2. Mapped shape palette
test_plots$scatter_shape =
  ggplot(iris, aes(Sepal.Width, Sepal.Length, shape = Species)) +
  geom_point_plus(aes(fill = Petal.Length), colour = "black") +
  theme_plus() +
  gridlines_plus() +
  yaxis_title_plus() +
  scale_continuous_plus(scale = "x") +
  scale_continuous_plus(scale = "y") +
  scale_continuous_plus(scale = "fill")

# 3. User overrides geom defaults
test_plots$scatter_overrides =
  ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
  geom_point_plus(shape = "flower", fill = "white", colour = "black", size = 7, alpha = 0.6) +
  theme_plus() +
  gridlines_plus() +
  yaxis_title_plus() +
  scale_continuous_plus(scale = "x") +
  scale_continuous_plus(scale = "y")

# 4. Global mapping inherited by geom_point_plus
test_plots$global_mapping =
  ggplot(iris, aes(Sepal.Width, Sepal.Length, fill = Species, shape = Species)) +
  geom_point_plus(colour = "black") +
  theme_plus() +
  gridlines_plus() +
  yaxis_title_plus() +
  scale_continuous_plus(scale = "x") +
  scale_continuous_plus(scale = "y")

# 5. Faceting: no yaxis_title_plus, but gridlines/scale/theme still active
test_plots$facet_scatter =
  ggplot(iris, aes(Sepal.Width, Sepal.Length, fill = Petal.Length)) +
  geom_point_plus(shape = "squircle", colour = "black") +
  facet_wrap(~ Species) +
  theme_plus() +
  gridlines_plus() +
  scale_continuous_plus(scale = "x") +
  scale_continuous_plus(scale = "y") +
  scale_continuous_plus(scale = "fill")

# 6. Boxplot defaults + continuous y
test_plots$boxplot =
  ggplot(iris, aes(Species, Sepal.Length)) +
  geom_boxplot() +
  theme_plus() +
  gridlines_plus() +
  yaxis_title_plus() +
  scale_continuous_plus(scale = "y", name = "Sepal length")

# 7. Violin defaults: quantiles/fill behavior
test_plots$violin =
  ggplot(iris, aes(Species, Sepal.Length)) +
  geom_violin() +
  theme_plus() +
  gridlines_plus() +
  yaxis_title_plus() +
  scale_continuous_plus(scale = "y")

# 8. Bar/col defaults: transparent fill behavior
test_plots$col =
  ggplot(mtcars, aes(factor(cyl), mpg)) +
  stat_summary(geom = "col", fun = mean) +
  theme_plus() +
  gridlines_plus() +
  yaxis_title_plus() +
  scale_continuous_plus(scale = "y", name = "Mean mpg")

# 9. Histogram defaults + continuous x/y
test_plots$histogram =
  ggplot(mtcars, aes(mpg)) +
  geom_histogram(bins = 12) +
  theme_plus() +
  gridlines_plus() +
  yaxis_title_plus() +
  scale_continuous_plus(scale = "x") +
  scale_continuous_plus(scale = "y", name = "Count")

# 10. Density defaults
test_plots$density =
  ggplot(mtcars, aes(mpg)) +
  geom_density() +
  theme_plus() +
  gridlines_plus() +
  yaxis_title_plus() +
  scale_continuous_plus(scale = "x") +
  scale_continuous_plus(scale = "y")

# 11. Smooth ribbon defaults
test_plots$smooth =
  ggplot(mtcars, aes(wt, mpg)) +
  geom_point_plus(shape = "oval") +
  geom_smooth(method = "lm", formula = y ~ x) +
  theme_plus() +
  gridlines_plus() +
  yaxis_title_plus() +
  scale_continuous_plus(scale = "x") +
  scale_continuous_plus(scale = "y")

# 12. Ribbon/area defaults
test_plots$ribbon =
  ggplot(ggplot2::economics, aes(date, unemploy)) +
  geom_ribbon(aes(ymin = unemploy - 500, ymax = unemploy + 500)) +
  geom_line() +
  theme_plus() +
  gridlines_plus() +
  yaxis_title_plus() +
  scale_continuous_plus(scale = "y", name = "Unemployment")

# 13. Tile defaults + fill scale
test_plots$tile =
  ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
  geom_tile() +
  theme_plus() +
  gridlines_plus() +
  yaxis_title_plus() +
  scale_continuous_plus(scale = "x") +
  scale_continuous_plus(scale = "y") +
  scale_continuous_plus(scale = "fill")

# 14. Custom labels vector on scale_continuous_plus
test_plots$custom_labels =
  ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
  geom_point_plus(shape = "cross") +
  theme_plus() +
  gridlines_plus() +
  yaxis_title_plus() +
  scale_continuous_plus(scale = "x") +
  scale_continuous_plus(
    scale = "y",
    name = "Custom y labels",
    labels = LETTERS[1:5],
    thin.labels = FALSE
  )

# 15. Thin labels
test_plots$thin_labels =
  ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
  geom_point_plus(shape = "octagon") +
  theme_plus() +
  gridlines_plus() +
  yaxis_title_plus() +
  scale_continuous_plus(scale = "x", thin.labels = TRUE) +
  scale_continuous_plus(scale = "y", thin.labels = TRUE)

# 16. Coord flip: yaxis_title_plus should still kill/move correct label
test_plots$coord_flip =
  ggplot(iris, aes(Species, Sepal.Length)) +
  geom_boxplot() +
  coord_flip() +
  theme_plus() +
  gridlines_plus() +
  yaxis_title_plus() +
  scale_continuous_plus(scale = "y", name = "Sepal length")

# 17. Top legend interaction
test_plots$top_legend =
  ggplot(iris, aes(Sepal.Width, Sepal.Length, fill = Species, shape = Species)) +
  geom_point_plus(colour = "black") +
  theme_plus(legend.position = "top") +
  gridlines_plus() +
  yaxis_title_plus(nudgeTopLegendDown = TRUE) +
  scale_continuous_plus(scale = "x") +
  scale_continuous_plus(scale = "y")

# 18. Multiple continuous aesthetics
test_plots$multi_aes =
  ggplot(iris, aes(Sepal.Width, Sepal.Length, fill = Petal.Length, size = Petal.Width)) +
  geom_point_plus(shape = "waffle", colour = "black") +
  theme_plus() +
  gridlines_plus() +
  yaxis_title_plus() +
  scale_continuous_plus(scale = "x") +
  scale_continuous_plus(scale = "y") +
  scale_continuous_plus(scale = "fill")

# Smoke test: build all plots
# plot_build_results = lapply(names(test_plots), function(nm) {
#   message("Building: ", nm)
#   ggplot2::ggplot_build(test_plots[[nm]])
# })

# Optional visual pass
# for(nm in names(test_plots)) {
#   message("Printing: ", nm)
#   print(test_plots[[nm]])
# }

# Optional save pass
# dir.create("ggplot_plus_test_outputs", showWarnings = FALSE)
# for(nm in names(test_plots)) {
#   ggsave(
#     filename = file.path("ggplot_plus_test_outputs", paste0(nm, ".png")),
#     plot = test_plots[[nm]],
#     width = 7,
#     height = 5,
#     dpi = 150
#   )
# }
