% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_point_plus.R
\docType{data}
\name{GeomPointPlus}
\alias{GeomPointPlus}
\title{An alternative version of \code{ggplot2}'s geomPoint proto that incorporates new, distinctive shapes.}
\format{
An object of class \code{PointPlus} (inherits from \code{GeomPoint}, \code{Geom}, \code{ggproto}, \code{gg}) of length 4.
}
\usage{
GeomPointPlus
}
\value{
A \code{ggplot2} ggproto subclass object.
}
\description{
This ggplot proto object is called internally by \code{geom_point2()} and inherits most, but not all, of its methods and fields from those used in \code{ggplot2}'s standard \code{geomPoint} proto class. However, it has different default aesthetics, a different shapes palette, and can draw these new shapes in a legend. This subclass is not meant to be encountered by the user and is instead fodder for \code{geom_point_plus()}.
}
\keyword{datasets}
