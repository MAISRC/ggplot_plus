# README
Dr. Alex Bajcz, Quantitative Ecologist, Minnesota Aquatic Invasive
Species Research Center
2025-07-09

## Quick Start Guide

To begin using the `ggplot.plus` package, you’ll first need to install
it from GitHub using the devtools package:

``` r
# install.packages("devtools")  # if not already installed
devtools::install_github("https://github.com/MAISRC/ggplot_plus") #<--NOTE THE _ INSTEAD OF THE . IN THE NAME.
```

Then, load it alongside `ggplot2`:

``` r
# install.packages("ggplot2")  # if not already installed
library(ggplot2)
library(ggplot.plus)
```

Once loaded, you can start layering in the “plus” tools to improve your
plot design with minimal effort.

### Using the tools

Just by loading `ggplot.plus`, you will switch your session’s default
color palette to one that is more broadly accessible–there’s no need to
call `palettes_plus()` to alter this palette unless you want to (see the
“Accessible Color Palettes” section for details):

``` r
#A BASIC GGPLOT SCATTERPLOT, EXCEPT USING ggplot.plus's DEFAULT COLOR PALETTE INSTEAD.
ggplot(iris, 
       mapping = aes(x = Petal.Length, 
                     y = Sepal.Length)) +
  geom_point(mapping = aes(color = Species))
```

![](README_files/figure-commonmark/auto%20new%20palette-1.png)

To access the tweaks the package makes to `ggplot2`’s common geometries
(“geoms”), convert your `geom_*()` calls into `geom_plus()` calls,
including the name of the `geom` for the `geom` argument:

``` r
ggplot(iris, 
       mapping = aes(x = Petal.Length, 
                     y = Sepal.Length)) +
  geom_plus(geom = "point", #<--DIFF FUNCTION, NEW GEOM INPUT.
            mapping = aes(color = Species))
```

![](README_files/figure-commonmark/showing%20geom_plus-1.png)

You can already see that this is a very different graph than what
`ggplot2` would produce by default!

Everything else the package offers gets turned on when you add (using
`ggplot2`’s usual `+` operator) the associated function to your
`ggplot()` command:

``` r
ggplot(iris, 
       mapping = aes(x = Petal.Length, 
                     y = Sepal.Length)) +
  geom_plus(geom = "point", 
            mapping = aes(color = Species)) + 
  theme_plus() + #<-OVERHAULS VISUAL THEME
  scale_x_continuous_plus("Petal length (cm)",
                          thin_labels = TRUE) + #<--OVERHAULS AXIS BREAKS AND LIMITS (FOR CONTINUOUS AXES ONLY!) 
  scale_y_continuous_plus("Sepal length (cm)") + #<--SAME FOR Y AXIS.
  yaxis_title_plus() + #<--RELOCATES AND RE-ORIENTS Y AXIS TITLE.
  gridlines_plus() + #<--ADDS THOUGHTFUL GRIDLINES, IF YOU *REALLY* WANT THEM.
  labs(color = expression(italic("Iris")*" species")) #<--THIS IS BASE GGPLOT2, BUT A NICE TOUCH!
```

![](README_files/figure-commonmark/quick%20start%20use-1.png)

The above graphs demonstrates how `ggplot.plus`’s tools rethink the
default design features of `ggplot2`. The intention is to yield a
cleaner, more accessible, and more modern baseline more quickly so that
you need to spend less time fine-tuning and polishing your graphs for
publication than you might otherwise need to.

However, there’s a *lot* more to know! If you want to dive deeper,
please check out the full package guide at insert URL here!
