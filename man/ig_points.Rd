% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grob.R
\name{ig_points}
\alias{ig_points}
\title{Create a grob}
\usage{
ig_points(
  x = 0.5,
  y = 0.5,
  pch = 1,
  size = 4,
  default.units = getOption("ingrid.default.units", "npc"),
  name = NULL,
  gp = NULL,
  vp = NULL,
  col,
  fill,
  alpha,
  lty,
  lwd,
  lex,
  lineend,
  linejoin,
  linemitre,
  fontsize,
  cex,
  fontfamily,
  fontface,
  lineheight,
  mask,
  clip,
  layout,
  layout.pos.row,
  layout.pos.col
)
}
\arguments{
\item{x, y}{x,y location}

\item{pch}{numeric or character vector indicating what sort of plotting
symbol to use. See points for the interpretation of these values,
and note fill below.}

\item{size}{unit object specifying the size of the plotting symbols.}

\item{default.units}{\code{getOption("ingrid.default.units", 'npc')}}

\item{name}{grob name. default: NULL}

\item{gp}{graphical parameter object created by \code{grid::gpar()} or
\code{ingrid::gp()}.  If NULL (default), then a \code{gpar} object
is created from the relevant arguments to this function i.e. \code{fill},
\code{col}, etc}

\item{vp}{viewport object created by \code{grid::viewport()}.
If NULL (default), then a \code{viewport} object
is created from a limited subset of arguments to this function i.e. \code{mask},
\code{clip}, etc}

\item{col, fill, alpha, lty, lwd, lex, lineend, linejoin, linemitre, fontsize, cex, fontfamily, fontface, lineheight}{See documentation for \code{gp()}}

\item{mask, clip, layout, layout.pos.row, layout.pos.col}{See documentation for
\code{grid::viewport()}}
}
\description{
Create a grob
}
\seealso{
Other grobs: 
\code{\link{ig_bbox}()},
\code{\link{ig_bezier}()},
\code{\link{ig_circle}()},
\code{\link{ig_lines}()},
\code{\link{ig_null}()},
\code{\link{ig_path}()},
\code{\link{ig_polygon}()},
\code{\link{ig_polyline}()},
\code{\link{ig_raster}()},
\code{\link{ig_roundrect}()},
\code{\link{ig_segments}()},
\code{\link{ig_text}()}
}
\concept{grobs}
