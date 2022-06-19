
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' grob operations using \code{gridGeometry::polyclipGrob()}
#'
#' This wrapper sets the graphical parameters on the result to be those of
#' the \code{x} argument.
#'
#' @param op operation. One of 'intersection', 'xor', 'union', 'minus'
#' @param x,y grobs, gtrees of polyclipgrob objects
#' @param gp gpar settings. default \code{x$gp}
#' @param name default NULL
#'
#' @import grid
#' @import gridGeometry
#' @export
#'
#' @family combinators
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
igc_combine <- function(x, y, op = c('intersection', 'xor', 'union', 'minus'), gp = x$gp, name = NULL) {

  op <- match.arg(op)

  if (is_polyclipgrob(x)) {
    # message(op, " x polyclip")
    if (op == 'xor') {warning("Nested xor polyclip grobs don't do anything sensible")}
    x$A <- igc_combine(op, x$A, y)
    x$B <- igc_combine(op, x$B, y)
    x
  } else if (is_polyclipgrob(y)) {
    # message(op, ' y polyclip')
    if (op == 'xor') {warning("Nested xor polyclip grobs don't do anything sensible")}
    y$A <- igc_combine(op, y$A, x, gp = gp)
    y$B <- igc_combine(op, y$B, x, gp = gp)
    y$gp <- gp
    y
  } else if (is_vanilla_grob(x) && is_vanilla_grob(y)) {
    # message(op, ' x vanilla y vanilla')
    gridGeometry::polyclipGrob(x, y, op = op, gp = gp, name = name)
  } else if (is_vanilla_gTree(x)) {
    # message(op, ' x gtree')
    grobs <- lapply(x$children, function(.) {
      igc_combine(op, x = ., y = y)
    })
    do.call(grid::grobTree, grobs)
  } else if (is_vanilla_gTree(y)) {
    # message(op, ' y gtree')
    grobs <- lapply(y$children, function(.) {
      igc_combine(x = ., y = x, gp = x$gp)
    })
    do.call(grid::grobTree, grobs)
  } else {
    stop(op, " igc_combine not handled: ", deparse(class(x)), " - ", deparse(class(y)))
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname igc_combine
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
igc_intersect <- function(x, y, gp=x$gp, name = NULL) {
  igc_combine('intersection', x=x, y=y, gp=gp, name=name)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname igc_combine
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
igc_union <- function(x, y, gp=x$gp, name = NULL) {
  igc_combine('union', x=x, y=y, gp=gp, name=name)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname igc_combine
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
igc_minus <- function(x, y, gp=x$gp, name = NULL) {
  igc_combine('minus', x=x, y=y, gp=gp, name=name)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname igc_combine
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
igc_xor <- function(x, y, gp=x$gp, name = NULL) {
  igc_combine('xor', x=x, y=y, gp=gp, name=name)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Invert the geometry of a grob
#'
#' @param x grob
#' @param name default: NULL
#'
#' @export
#'
#' @family combinators
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
igc_invert <- function(x, name = NULL) {
  igc_minus(
    rectGrob(),
    x,
    gp = x$gp
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Stack multiple grobs toegher into a grobTree
#'
#' @param ... grobs
#' @param name default: NULL
#'
#' @export
#'
#' @family combinators
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
igc_stack <- function(..., name = NULL) {
  grobTree(..., name = name)
}


