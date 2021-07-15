


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Simple wrapper for \code{grid::gpar()} which exposes the valid arguments
#'
#' The \code{grid::gpar()} does not have any named arguments, and when used
#' in a modern IDE, it is difficult to quicklyl know which graphical parameters
#' are available.
#'
#' This function (\code{ingrid::gp()}) is a thin wrapper around \code{grid::gpar()}
#' which exposes all the parameters as named arguments to make autocomplete
#' a bit more useful.
#'
#' @param col Colour for lines and borders.
#' @param fill Colour for filling rectangles, polygons, ...
#' @param alpha Alpha channel for transparency
#' @param lty Line type
#' @param lwd Line width
#' @param lex Multiplier applied to line width
#' @param lineend Line end style ('round', 'butt', 'square')
#' @param linejoin Line join style ('round', 'mitre', 'bevel')
#' @param linemitre Line mitre limit (number greater than 1)
#' @param fontsize The size of text (in points)
#' @param cex Multiplier applied to fontsize
#' @param fontfamily The font family
#' @param fontface The font face ('bold', 'italic', ...)
#' @param lineheight The height of a line as a multiple of the size of text
#' @param ... other arguments ignored
#'
#' @examples
#' \dontrun{
#' gp(fill = 'red')
#' }
#'
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gp <- function(col, fill, alpha, lty, lwd, lex, lineend, linejoin, linemitre,
               fontsize, cex, fontfamily, fontface, lineheight, ...) {

  args <- find_args()
  do.call(grid::gpar, args)
}
