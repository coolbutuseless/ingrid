

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title dotmm
#' @name  dotmm
#' @description Specialized functions for unit creation
#'
#' @param x numeric vector
#'
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.mm <- function(x) { grid::unit(x, 'mm') }

#' @rdname dotmm
#' @export
.npc <- function(x) { grid::unit(x, 'npc') }

#' @rdname dotmm
#' @export
.inch <- function(x) { grid::unit(x, 'inches') }

#' @rdname dotmm
#' @export
.cm <- function(x) { grid::unit(x, 'cm') }

#' @rdname dotmm
#' @export
.points <- function(x) { grid::unit(x, 'points') } # avoid name clash with ggplot2

#' @rdname dotmm
#' @export
.snpc <- function(x) { grid::unit(x, 'snpc') }




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Shortcut value for `unit(0.5, 'npc')`
#'
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.mid <- grid::unit(0.5, 'npc')
