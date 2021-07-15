
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' grob operations using \code{gridGeometry::polyclipGrob()}
#'
#' This wrapper sets the gp on the result to be the gp of the first argument.
#'
#' @param op operation. One of 'intersection', 'xor', 'union', 'minus'
#' @param x,y grobs, gtrees of polyclipgrob objects
#' @param gp gpar settings. default \code{x$gp}
#' @param name default NULL
#'
#' @import grid
#' @import gridGeometry
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
grob_combine <- function(op = c('intersection', 'xor', 'union', 'minus'), x, y, gp = x$gp, name = NULL) {

  op <- match.arg(op)

  if (is_polyclipgrob(x)) {
    # message(op, " x polyclip")
    if (op == 'xor') {warning("Nested xor polyclip grobs don't do anything sensible")}
    x$A <- grob_combine(op, x$A, y)
    x$B <- grob_combine(op, x$B, y)
    x
  } else if (is_polyclipgrob(y)) {
    # message(op, ' y polyclip')
    if (op == 'xor') {warning("Nested xor polyclip grobs don't do anything sensible")}
    y$A <- grob_combine(op, y$A, x, gp = gp)
    y$B <- grob_combine(op, y$B, x, gp = gp)
    y$gp <- gp
    y
  } else if (is_vanilla_grob(x) && is_vanilla_grob(y)) {
    # message(op, ' x vanilla y vanilla')
    gridGeometry::polyclipGrob(x, y, op = op, gp = gp, name = name)
  } else if (is_vanilla_gTree(x)) {
    # message(op, ' x gtree')
    grobs <- lapply(x$children, function(.) {
      grob_combine(op, x = ., y = y)
    })
    do.call(grid::grobTree, grobs)
  } else if (is_vanilla_gTree(y)) {
    # message(op, ' y gtree')
    grobs <- lapply(y$children, function(.) {
      grob_combine(x = ., y = x, gp = x$gp)
    })
    do.call(grid::grobTree, grobs)
  } else {
    stop(op, " grob_combine not handled: ", deparse(class(x)), " - ", deparse(class(y)))
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname grob_combine
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
grob_intersect <- function(x, y, gp=x$gp, name = NULL) {
  grob_combine('intersection', x=x, y=y, gp=gp, name=name)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname grob_combine
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
grob_union <- function(x, y, gp=x$gp, name = NULL) {
  grob_combine('union', x=x, y=y, gp=gp, name=name)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname grob_combine
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
grob_minus <- function(x, y, gp=x$gp, name = NULL) {
  grob_combine('minus', x=x, y=y, gp=gp, name=name)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname grob_combine
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
grob_xor <- function(x, y, gp=x$gp, name = NULL) {
  grob_combine('xor', x=x, y=y, gp=gp, name=name)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Invert the geometry of a grob
#'
#' @param x grob
#' @param name default: NULL
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
grob_invert <- function(x, name = NULL) {
  grob_minus(
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
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
grob_stack<- function(..., name = NULL) {
  grobTree(..., name = name)
}


