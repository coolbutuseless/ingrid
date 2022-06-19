



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Rotate a grob or viewport
#'
#' @param obj grob or viewport
#' @param angle rotation angle in degrees
#'
#' @import grid
#' @export
#'
#' @family transformations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
igt_rotate <- function(obj, angle) {

  if (is_grob(obj)) {
    if (is.null(obj$vp)) {
      obj$vp <- vpc()
    }
    obj$vp$angle <- obj$vp$angle + angle
  } else if (is_viewport(obj)) {
    obj$angle <- obj$angle + angle
  } else {
    stop("must be grob or viewport")
  }

  grob_auto_name(obj)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Translate a grob or viewport
#'
#' @param obj grob or viewport
#' @param x,y translation
#' @param default.units 'npc'
#'
#' @import grid
#' @export
#'
#' @family transformations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
igt_translate <- function(obj, x = 0, y = 0, default.units = getOption("ingrid.default.units", 'npc')) {

  x <- make_unit(x, default.units)
  y <- make_unit(y, default.units)

  if (is_grob(obj)) {
    if (is.null(obj$vp)) {
      obj$vp <- vpc()
    }

    obj$vp$x <- obj$vp$x + x
    obj$vp$y <- obj$vp$y + y
  } else if (is_viewport(obj)) {
    obj$x <- obj$x + x
    obj$y <- obj$y + y
  } else {
    stop("must be grob or viewport")
  }

  grob_auto_name(obj)
}


gp_arg_names <- c('col', 'fill', 'alpha', 'lty', 'lwd', 'lex', 'lineend',
                  'linejoin', 'linemitre', 'fontsize', 'cex', 'fontfamily',
                  'fontface', 'lineheight')

vp_arg_names <- c('mask', 'clip', 'layout', 'layout.pos.row', 'layout.pos.col')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Update a grob/viewport
#'
#' Note: No checking is currently done on the validity of these parameters.
#'       Use with caution!
#'
#' @param obj grob or viewport
#' @param ... named arguments e.g. \code{fill = 'red'}
#'
#' @import grid
#' @export
#'
#' @family transformations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
igt_update <- function(obj, ...) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check all updated arguments are named
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  args <- list(...)
  if (is.null(names(args)) || any(names(args) == '')) {
    stop("igt_update(): All arguments must be named")
  }

  if (is_grob(obj)) {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Update values in the graphical parameters
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (is.null(obj$gp)) { obj$gp <- gp()}
    gp_arg_names <- intersect(gp_arg_names, names(args))
    for (gp_name in gp_arg_names) {
      obj$gp[[gp_name]] <- args[[gp_name]]
    }


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Update values in the viewport
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (is.null(obj$vp)) { obj$vp <- vpc()}
    vp_arg_names <- intersect(vp_arg_names, names(args))
    for (vp_name in vp_arg_names) {
      obj$vp[[vp_name]] <- args[[vp_name]]
    }
  }

  arg_names <- setdiff(names(args), c(gp_arg_names, vp_arg_names))
  for (arg_name in arg_names) {
    obj[[arg_name]] <- args[[arg_name]]
  }

  grob_auto_name(obj)
}







