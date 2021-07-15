


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Translate the (x,y) coordinates of a  grob
#'
#' @param grob grob
#' @param x,y translation
#' @param default.units getOption("ingrid.default.units", 'mm')
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
grob_translate <- function(grob, x, y, default.units = getOption("ingrid.default.units", 'mm')) {

  if (!('x' %in% names(grob) && 'y' %in% names(grob))) {
    stop("grob does not have 'x' and 'y' paramters to translate!")
  }

  x <- make_unit(x, default.units)
  y <- make_unit(y, default.units)

  grob$x <- grob$x + x
  grob$y <- grob$y + y

  grob_auto_name(grob)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Rotate the (x,y) coordinates of a grob
#'
#' @param grob grob
#' @param angle rotation angle
#' @param default.units 'npc'
#' @param xc,yc centre of rotation.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
grob_rotate <- function(grob, angle, xc = unit(0.5, 'npc'),
                        yc = unit(0.5, 'npc'), default.units = 'npc') {


  xc <- make_unit(xc, default.units)
  yc <- make_unit(yc, default.units)

  grob <- grob_translate(grob, -xc, -yc)

  if (!('x' %in% names(grob) && 'y' %in% names(grob))) {
    stop("grob does not have 'x' and 'y' paramters to rotate!")
  }

  theta <- angle * pi / 180

  x <- grob$x
  y <- grob$y
  grob$x <- x * cos(theta) - y * sin(theta)
  grob$y <- x * sin(theta) + y * cos(theta)


  grob <- grob_translate(grob, xc, yc)

  grob_auto_name(grob)
}




