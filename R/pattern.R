

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Alternate call for \code{grid::pattern()} with different defaults
#'
#' @param grob grob
#' @param x,y default 0,0
#' @param width,height default: 20,20
#' @param default.units getOption("ingrid.default.units", 'mm')
#' @param just justification. default: centre
#' @param hjust,vjust other justification parameters
#' @param extend default: 'repeat'
#' @param gp graphical parameters
#' @param centred default: TRUE. (x, y) will be offset from (0.5npc, 0.5npc)
#'
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pattern_create <- function(
  grob,
  x      = 0,
  y      = 0,
  width  = 20,
  height = 20,
  default.units = getOption("ingrid.default.units", 'mm'),
  just    = 'centre',
  hjust   = NULL,
  vjust   = NULL,
  extend  = c('repeat', 'pad', 'reflect', 'none'),
  gp      = gpar(fill = 'transparent'),
  centred = TRUE
) {

  x      <- make_unit(x     , default.units)
  y      <- make_unit(y     , default.units)
  width  <- make_unit(width , default.units)
  height <- make_unit(height, default.units)

  if (isTRUE(centred)) {
    x <- x + unit(0.5, 'npc')
    y <- y + unit(0.5, 'npc')
  }


  grid::pattern(
    grob          = grob,
    x             = x,
    y             = y,
    width         = width,
    height        = height,
    default.units = default.units,
    just          = just,
    hjust         = hjust,
    vjust         = vjust,
    extend        = match.arg(extend),
    gp            = gp
  )

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' view a pattern
#'
#' @param pat pattern object
#' @param width,height dimenaions of rectangle holding pattern
#' @param clear should the page be clared before drawing pattern? default: TRUE
#' @param ... other arguments passed to \code{rect_grob}
#'
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pattern_view <- function(pat, height = .mm(50), width = .mm(50 * 1.618), clear = TRUE, ...) {

  big <- rect_grob(width = width, height = height, ...)
  big$gp$fill <- pat

  if (isTRUE(clear)) {
    grid.newpage()
  }

  grid.draw(big)

  invisible(pat)
}




