

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
.points <- function(x) { grid::unit(x, 'points') }

#' @rdname dotmm
#' @export
.snpc <- function(x) { grid::unit(x, 'snpc') }
