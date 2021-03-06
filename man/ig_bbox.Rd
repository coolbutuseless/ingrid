% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grob.R
\name{ig_bbox}
\alias{ig_bbox}
\alias{ig_rect}
\title{Create a rect grob}
\usage{
ig_bbox(
  x = 0,
  y = 0,
  width = 1,
  height = 1,
  just = c("left", "bottom"),
  hjust = NULL,
  vjust = NULL,
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

ig_rect(
  x = 0.5,
  y = 0.5,
  width = 1,
  height = 1,
  just = "centre",
  hjust = NULL,
  vjust = NULL,
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

\item{width, height}{extents}

\item{just}{The justification of the shape relative to its (x, y)
location. If there are two values, the first value specifies horizontal
justification and the second value specifies vertical justification.
Possible string values are: "left", "right", "centre",
"center", "bottom", and "top". For numeric values, 0 means left
alignment and 1 means right alignment.}

\item{hjust}{A numeric vector specifying horizontal justification.
If specified, overrides the just setting.}

\item{vjust}{A numeric vector specifying vertical justification.
If specified, overrides the just setting.}

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
\code{ig_bbox} is a simple variant where the (x, y) coords default to
defining the bottom left corner.  This can be easier to reason about in
some cases.
}
\examples{
\dontrun{
ig_bbox(width = 40, fill = 'red')
}

}
\seealso{
Other grobs: 
\code{\link{ig_bezier}()},
\code{\link{ig_circle}()},
\code{\link{ig_lines}()},
\code{\link{ig_null}()},
\code{\link{ig_path}()},
\code{\link{ig_points}()},
\code{\link{ig_polygon}()},
\code{\link{ig_polyline}()},
\code{\link{ig_raster}()},
\code{\link{ig_roundrect}()},
\code{\link{ig_segments}()},
\code{\link{ig_text}()}
}
\concept{grobs}
