


if (FALSE) {
  all_grobs <- c(
    "null",
    "circle",
    "rect",
    "polygon",
    "polyline",
    "lines",
    "roundrect",
    "segments",
    "pathgrob",
    "xspline",
    "beziergrob",
    "rastergrob",
    "text",
    "points",
    "clip",
    "functiongrob",
    "frame",
    "cellGrob"
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     #      #
#     #      #
#     ####   ####    ###   #   #
#     #   #  #   #  #   #   # #
#     #   #  #   #  #   #    #
#     #   #  #   #  #   #   # #
#     ####   ####    ###   #   #
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a rect grob
#'
#'
#' \code{bbox_grob} is a simple variant where the (x, y) coords default to
#' defining the bottom left corner.  This can be easier to reason about in
#' some cases.
#'
#' @param x,y x,y location
#' @param width,height extents
#' @param just The justification of the shape relative to its (x, y)
#'        location. If there are two values, the first value specifies horizontal
#'        justification and the second value specifies vertical justification.
#'        Possible string values are: "left", "right", "centre",
#'        "center", "bottom", and "top". For numeric values, 0 means left
#'        alignment and 1 means right alignment.
#' @param hjust A numeric vector specifying horizontal justification.
#'        If specified, overrides the just setting.
#' @param vjust A numeric vector specifying vertical justification.
#'        If specified, overrides the just setting.


#'
#' @param default.units \code{getOption("ingrid.default.units", 'mm')}
#' @param name grob name. default: NULL
#' @param gp graphical parameter object created by \code{grid::gpar()} or
#'        \code{ingrid::gp()}.  If NULL (default), then a \code{gpar} object
#'        is created from the relevant arguments to this function i.e. \code{fill},
#'        \code{col}, etc
#' @param vp viewport object created by \code{grid::viewport()} or
#'        \code{ingrid::vp()}.  If NULL (default), then a \code{viewport} object
#'        is created from a limited subset of arguments to this function i.e. \code{mask},
#'        \code{clip}, etc
#' @param centred default: TRUE to shift given x,y coordinates by
#'        c(0.5npc, 0.5npc)
#' @param col,fill,alpha,lty,lwd,lex,lineend,linejoin,linemitre,fontsize,cex,fontfamily,fontface,lineheight
#'        See documentation for \code{gp()}
#' @param mask,clip,layout,layout.pos.row,layout.pos.col See documentation for
#'        \code{grid::viewport()}
#'
#' @examples
#' \dontrun{
#' bbox_grob(width = 40, fill = 'red')
#' }
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bbox_grob <- function(
  x      =  0,
  y      =  0,
  width  = 20,
  height = 20,
  just   = c('left', 'bottom'),
  hjust  = NULL,
  vjust  = NULL,
  default.units = getOption("ingrid.default.units", 'mm'),
  name    = NULL,
  gp      = NULL,
  vp      = NULL,
  centred = TRUE,
  col, fill, alpha, lty, lwd, lex, lineend, linejoin, linemitre, fontsize, cex, fontfamily, fontface, lineheight,
  mask,clip,layout,layout.pos.row,layout.pos.col
) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create 'gp' and 'vp' if not given
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  args <- find_args()
  if (is.null(gp)) {
    gp <- do.call(ingrid::gp, args)
  }

  if (is.null(vp)) {
    vp_arg_names <- c('mask', 'clip', 'layout', 'layout.pos.row', 'layout.pos.col')
    vp_arg_names <- intersect(vp_arg_names, names(args))
    vp_args <- args[vp_arg_names]
    vp <- do.call(ingrid::vp, vp_args)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # create the grob
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rect_grob(
    x             = x,
    y             = y,
    width         = width,
    height        = height,
    just          = just,
    hjust         = hjust,
    vjust         = vjust,
    default.units = default.units,
    name          = name,
    gp            = gp,
    vp            = vp,
    centred       = centred
  )
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     #                      #
#     #
#     ####    ###   #####   ##     ###   # ##
#     #   #  #   #     #     #    #   #  ##
#     #   #  #####    #      #    #####  #
#     #   #  #       #       #    #      #
#     ####    ###   #####   ###    ###   #
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a grob
#'
#' @param x,y locations of spline control points
#' @param id A numeric vector used to separate locations in x and y into
#'        multiple beziers. All locations with the same id belong to the
#'        same bezier.
#' @param id.lengths A numeric vector used to separate locations in x and y
#'        into multiple bezier. Specifies consecutive blocks of locations
#'        which make up separate beziers.
#' @param arrow A list describing arrow heads to place at either end of
#'        the bezier, as produced by the arrow function.
#'
#' @inheritParams bbox_grob
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bezier_grob <- function(
  x             = c(0, 0.5, 1, 0.5) * 20,
  y             = c(0.5, 1, 0.5, 0) * 20,
  id            = NULL,
  id.lengths    = NULL,
  default.units = getOption("ingrid.default.units", 'mm'),
  arrow         = NULL,
  name          = NULL,
  gp            = NULL,
  vp            = NULL,
  centred       = TRUE,
  col, fill, alpha, lty, lwd, lex, lineend, linejoin, linemitre, fontsize, cex, fontfamily, fontface, lineheight,
  mask,clip,layout,layout.pos.row,layout.pos.col
) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create 'gp' and 'vp' if not given
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  args <- find_args()
  if (is.null(gp)) {
    gp <- do.call(ingrid::gp, args)
  }

  if (is.null(vp)) {
    vp_arg_names <- c('mask', 'clip', 'layout', 'layout.pos.row', 'layout.pos.col')
    vp_arg_names <- intersect(vp_arg_names, names(args))
    vp_args <- args[vp_arg_names]
    vp <- do.call(ingrid::vp, vp_args)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Standard unit handling
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x <- make_unit(x, default.units)
  y <- make_unit(y, default.units)

  if (isTRUE(centred)) {
    x <- x + unit(0.5, 'npc')
    y <- y + unit(0.5, 'npc')
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the grob
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  grid::bezierGrob(
    x          = x,
    y          = y,
    id         = id,
    id.lengths = id.lengths,
    arror      = arrow,
    name       = name,
    gp         = gp,
    vp         = vp,
    default.units = default.units
  )
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#              #                   ##
#                                   #
#      ####   ##    # ##    ####    #     ###
#     #        #    ##     #        #    #   #
#     #        #    #      #        #    #####
#     #        #    #      #        #    #
#      ####   ###   #       ####   ###    ###
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a grob
#'
#' @param x,y coordinates of centre. default: c(0, 0)
#' @param r radius
#' @inheritParams bbox_grob
#'
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
circle_grob <- function(
  r             = 10,
  x             =  0,
  y             =  0,
  default.units = getOption("ingrid.default.units", 'mm'),
  name          = NULL,
  gp            = NULL,
  vp            = NULL,
  centred       = TRUE,
  col, fill, alpha, lty, lwd, lex, lineend, linejoin, linemitre, fontsize, cex, fontfamily, fontface, lineheight,
  mask,clip,layout,layout.pos.row,layout.pos.col
) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create 'gp' and 'vp' if not given
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  args <- find_args()
  if (is.null(gp)) {
    gp <- do.call(ingrid::gp, args)
  }

  if (is.null(vp)) {
    vp_arg_names <- c('mask', 'clip', 'layout', 'layout.pos.row', 'layout.pos.col')
    vp_arg_names <- intersect(vp_arg_names, names(args))
    vp_args <- args[vp_arg_names]
    vp <- do.call(ingrid::vp, vp_args)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Standard unit handling
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x <- make_unit(x, default.units)
  y <- make_unit(y, default.units)
  r <- make_unit(r, default.units)

  if (isTRUE(centred)) {
    x <- x + unit(0.5, 'npc')
    y <- y + unit(0.5, 'npc')
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the grob
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  grid::circleGrob(
    x    = x,
    y    = y,
    r    = r,
    name = name,
    gp   = gp,
    vp   = vp,
    default.units = default.units
  )
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#      ##      #
#       #
#       #     ##    # ##    ###    ####
#       #      #    ##  #  #   #  #
#       #      #    #   #  #####   ###
#       #      #    #   #  #          #
#      ###    ###   #   #   ###   ####
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a grob
#'
#' @inheritParams bbox_grob
#' @inheritParams bezier_grob
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lines_grob <- function(
  x      = c(0, 20),
  y      = c(0, 20),
  default.units = getOption("ingrid.default.units", 'mm'),
  arrow   = NULL,
  name    = NULL,
  gp      = NULL,
  vp      = NULL,
  centred = TRUE,
  col, fill, alpha, lty, lwd, lex, lineend, linejoin, linemitre, fontsize, cex, fontfamily, fontface, lineheight,
  mask,clip,layout,layout.pos.row,layout.pos.col
) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create 'gp' and 'vp' if not given
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  args <- find_args()
  if (is.null(gp)) {
    gp <- do.call(ingrid::gp, args)
  }

  if (is.null(vp)) {
    vp_arg_names <- c('mask', 'clip', 'layout', 'layout.pos.row', 'layout.pos.col')
    vp_arg_names <- intersect(vp_arg_names, names(args))
    vp_args <- args[vp_arg_names]
    vp <- do.call(ingrid::vp, vp_args)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Standard unit handling
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x <- make_unit(x, default.units)
  y <- make_unit(y, default.units)

  if (isTRUE(centred)) {
    x <- x + unit(0.5, 'npc')
    y <- y + unit(0.5, 'npc')
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the grob
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  grid::linesGrob(
    x     = x,
    y     = y,
    arrow = arrow,
    name  = name,
    gp    = gp,
    vp    = vp,
    default.units = default.units
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                    ##     ##
#                     #      #
#     # ##   #   #    #      #
#     ##  #  #   #    #      #
#     #   #  #   #    #      #
#     #   #  #  ##    #      #
#     #   #   ## #   ###    ###
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a grob
#'
#' @inheritParams bbox_grob
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
null_grob <- function(
  x      =  0,
  y      =  0,
  default.units = getOption("ingrid.default.units", 'mm'),
  name    = NULL,
  gp      = NULL,
  vp      = NULL,
  centred = TRUE,
  col, fill, alpha, lty, lwd, lex, lineend, linejoin, linemitre, fontsize, cex, fontfamily, fontface, lineheight,
  mask,clip,layout,layout.pos.row,layout.pos.col
) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create 'gp' and 'vp' if not given
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  args <- find_args()
  if (is.null(gp)) {
    gp <- do.call(ingrid::gp, args)
  }

  if (is.null(vp)) {
    vp_arg_names <- c('mask', 'clip', 'layout', 'layout.pos.row', 'layout.pos.col')
    vp_arg_names <- intersect(vp_arg_names, names(args))
    vp_args <- args[vp_arg_names]
    vp <- do.call(ingrid::vp, vp_args)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Standard unit handling
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x <- make_unit(x, default.units)
  y <- make_unit(y, default.units)

  if (isTRUE(centred)) {
    x <- x + unit(0.5, 'npc')
    y <- y + unit(0.5, 'npc')
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the grob
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  grid::nullGrob(
    x    = x,
    y    = y,
    name = name,
    vp   = vp,
    default.units = default.units
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                     #    #
#                     #    #
#     ####    ####  #####  ####
#     #   #  #   #    #    #   #
#     #   #  #   #    #    #   #
#     #   #  #  ##    #    #   #
#     ####    ## #     ##  #   #
#     #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a grob
#'
#' @param id A numeric vector used to separate locations in x and y into
#'        sub-objects. All locations with the same id belong to the same
#'        sub-object.
#' @param id.lengths A numeric vector used to separate locations in x and y
#'        into sub-objects. Specifies consecutive blocks of locations which
#'         make up separate sub-objects.
#' @param pathId A numeric vector used to separate locations in x and y
#'        into distinct objects. All locations with the same pathId belong
#'        to the same object.
#' @param pathId.lengths A numeric vector used to separate locations in
#'        x and y into objects. Specifies consecutive blocks of locations
#'        which make up separate objects.
#' @param rule A character value specifying the fill rule: either
#'        "winding" or "evenodd".
#'
#' @inheritParams bbox_grob
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
path_grob <- function(
  x              = c(0, 1, 1) * 20,
  y              = c(0, 0, 1) * 20,
  id             = NULL,
  id.lengths     = NULL,
  pathId         = NULL,
  pathId.lengths = NULL,
  rule           = c('winding', 'evenodd'),
  default.units  = getOption("ingrid.default.units", 'mm'),
  name           = NULL,
  gp             = NULL,
  vp             = NULL,
  centred        = TRUE,
  col, fill, alpha, lty, lwd, lex, lineend, linejoin, linemitre, fontsize, cex, fontfamily, fontface, lineheight,
  mask,clip,layout,layout.pos.row,layout.pos.col
) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create 'gp' and 'vp' if not given
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  args <- find_args()
  if (is.null(gp)) {
    gp <- do.call(ingrid::gp, args)
  }

  if (is.null(vp)) {
    vp_arg_names <- c('mask', 'clip', 'layout', 'layout.pos.row', 'layout.pos.col')
    vp_arg_names <- intersect(vp_arg_names, names(args))
    vp_args <- args[vp_arg_names]
    vp <- do.call(ingrid::vp, vp_args)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Standard unit handling
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x <- make_unit(x, default.units)
  y <- make_unit(y, default.units)

  if (isTRUE(centred)) {
    x <- x + unit(0.5, 'npc')
    y <- y + unit(0.5, 'npc')
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the grob
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  grid::pathGrob(
    x              = x,
    y              = y,
    id             = id,
    id.lengths     = id.lengths,
    pathId         = pathId,
    pathId.lengths = pathId.lengths,
    rule           = match.arg(rule),
    name           = name,
    gp             = gp,
    vp             = vp,
    default.units  = default.units
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                     #             #
#                                   #
#     ####    ###    ##    # ##   #####   ####
#     #   #  #   #    #    ##  #    #    #
#     #   #  #   #    #    #   #    #     ###
#     #   #  #   #    #    #   #    #        #
#     ####    ###    ###   #   #     ##  ####
#     #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a grob
#'
#' @param pch numeric or character vector indicating what sort of plotting
#'        symbol to use. See points for the interpretation of these values,
#'        and note fill below.
#' @param size unit object specifying the size of the plotting symbols.
#'
#' @inheritParams bbox_grob
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
points_grob <- function(
  x      = c(0, 1, 1) * 20,
  y      = c(0, 0, 1) * 20,
  pch    = 1,
  size   = 4,
  default.units = getOption("ingrid.default.units", 'mm'),
  name    = NULL,
  gp      = NULL,
  vp      = NULL,
  centred = TRUE,
  col, fill, alpha, lty, lwd, lex, lineend, linejoin, linemitre, fontsize, cex, fontfamily, fontface, lineheight,
  mask,clip,layout,layout.pos.row,layout.pos.col
) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create 'gp' and 'vp' if not given
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  args <- find_args()
  if (is.null(gp)) {
    gp <- do.call(ingrid::gp, args)
  }

  if (is.null(vp)) {
    vp_arg_names <- c('mask', 'clip', 'layout', 'layout.pos.row', 'layout.pos.col')
    vp_arg_names <- intersect(vp_arg_names, names(args))
    vp_args <- args[vp_arg_names]
    vp <- do.call(ingrid::vp, vp_args)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Standard unit handling
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x    <- make_unit(x   , default.units)
  y    <- make_unit(y   , default.units)
  size <- make_unit(size, default.units)

  if (isTRUE(centred)) {
    x <- x + unit(0.5, 'npc')
    y <- y + unit(0.5, 'npc')
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the grob
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  grid::pointsGrob(
    x    = x,
    y    = y,
    pch  = pch,
    size = size,
    name = name,
    gp   = gp,
    vp   = vp,
    default.units = default.units
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                    ##
#                     #
#     ####    ###     #    #   #   ####   ###   # ##
#     #   #  #   #    #    #   #  #   #  #   #  ##  #
#     #   #  #   #    #    #   #  #   #  #   #  #   #
#     #   #  #   #    #     ####   ####  #   #  #   #
#     ####    ###    ###       #      #   ###   #   #
#     #                     ###    ###
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a grob
#'
#' @inheritParams bbox_grob
#' @inheritParams path_grob
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
polygon_grob <- function(
  x             =  0,
  y             =  0,
  id            = NULL,
  id.lengths    = NULL,
  default.units = getOption("ingrid.default.units", 'mm'),
  name          = NULL,
  gp            = NULL,
  vp            = NULL,
  centred       = TRUE,
  col, fill, alpha, lty, lwd, lex, lineend, linejoin, linemitre, fontsize, cex, fontfamily, fontface, lineheight,
  mask,clip,layout,layout.pos.row,layout.pos.col
) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create 'gp' and 'vp' if not given
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  args <- find_args()
  if (is.null(gp)) {
    gp <- do.call(ingrid::gp, args)
  }

  if (is.null(vp)) {
    vp_arg_names <- c('mask', 'clip', 'layout', 'layout.pos.row', 'layout.pos.col')
    vp_arg_names <- intersect(vp_arg_names, names(args))
    vp_args <- args[vp_arg_names]
    vp <- do.call(ingrid::vp, vp_args)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Standard unit handling
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x <- make_unit(x, default.units)
  y <- make_unit(y, default.units)

  if (isTRUE(centred)) {
    x <- x + unit(0.5, 'npc')
    y <- y + unit(0.5, 'npc')
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the grob
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  grid::polygonGrob(
    x             = x,
    y             = y,
    id            = id,
    id.lengths    = id.lengths,
    name          = name,
    gp            = gp,
    vp            = vp,
    default.units = default.units
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                    ##            ##      #
#                     #             #
#     ####    ###     #    #   #    #     ##    # ##    ###
#     #   #  #   #    #    #   #    #      #    ##  #  #   #
#     #   #  #   #    #    #   #    #      #    #   #  #####
#     #   #  #   #    #     ####    #      #    #   #  #
#     ####    ###    ###       #   ###    ###   #   #   ###
#     #                     ###
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a grob
#'
#' @inheritParams bbox_grob
#' @inheritParams path_grob
#' @inheritParams bezier_grob
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
polyline_grob <- function(
  x             =  0,
  y             =  0,
  id            = NULL,
  id.lengths    = NULL,
  default.units = getOption("ingrid.default.units", 'mm'),
  arrow         = NULL,
  name          = NULL,
  gp            = NULL,
  vp            = NULL,
  centred       = TRUE,
  col, fill, alpha, lty, lwd, lex, lineend, linejoin, linemitre, fontsize, cex, fontfamily, fontface, lineheight,
  mask,clip,layout,layout.pos.row,layout.pos.col
) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create 'gp' and 'vp' if not given
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  args <- find_args()
  if (is.null(gp)) {
    gp <- do.call(ingrid::gp, args)
  }

  if (is.null(vp)) {
    vp_arg_names <- c('mask', 'clip', 'layout', 'layout.pos.row', 'layout.pos.col')
    vp_arg_names <- intersect(vp_arg_names, names(args))
    vp_args <- args[vp_arg_names]
    vp <- do.call(ingrid::vp, vp_args)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Standard unit handling
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x <- make_unit(x, default.units)
  y <- make_unit(y, default.units)

  if (isTRUE(centred)) {
    x <- x + unit(0.5, 'npc')
    y <- y + unit(0.5, 'npc')
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the grob
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  grid::polylineGrob(
    x             = x,
    y             = y,
    id            = id,
    id.lengths    = id.lengths,
    arrow         = arrow,
    name          = name,
    gp            = gp,
    vp            = vp,
    default.units = default.units
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                            #
#                            #
#     # ##    ####   ####  #####   ###   # ##
#     ##     #   #  #        #    #   #  ##
#     #      #   #   ###     #    #####  #
#     #      #  ##      #    #    #      #
#     #       ## #  ####      ##   ###   #
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a grob
#'
#' @param image Any R object that can be coerced to a raster object.
#' @param interpolate A logical value indicating whether to linearly
#'        interpolate the image (the alternative is to use nearest-neighbour
#'         interpolation, which gives a more blocky result). Default: FALSE
#'
#' @inheritParams bbox_grob
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
raster_grob <- function(
  image         = matrix(c(0, 1), 10, 10),
  x             =  0,
  y             =  0,
  width         = 20,
  height        = 20,
  just          = c('centre'),
  hjust         = NULL,
  vjust         = NULL,
  interpolate   = FALSE,
  default.units = getOption("ingrid.default.units", 'mm'),
  name          = NULL,
  gp            = NULL,
  vp            = NULL,
  centred       = TRUE,
  col, fill, alpha, lty, lwd, lex, lineend, linejoin, linemitre, fontsize, cex, fontfamily, fontface, lineheight,
  mask,clip,layout,layout.pos.row,layout.pos.col
) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create 'gp' and 'vp' if not given
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  args <- find_args()
  if (is.null(gp)) {
    gp <- do.call(ingrid::gp, args)
  }

  if (is.null(vp)) {
    vp_arg_names <- c('mask', 'clip', 'layout', 'layout.pos.row', 'layout.pos.col')
    vp_arg_names <- intersect(vp_arg_names, names(args))
    vp_args <- args[vp_arg_names]
    vp <- do.call(ingrid::vp, vp_args)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Standard unit handling
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x      <- make_unit(x     , default.units)
  y      <- make_unit(y     , default.units)
  width  <- make_unit(width , default.units)
  height <- make_unit(height, default.units)

  if (isTRUE(centred)) {
    x <- x + unit(0.5, 'npc')
    y <- y + unit(0.5, 'npc')
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the grob
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  grid::rasterGrob(
    image         = image,
    x             = x,
    y             = y,
    width         = width,
    height        = height,
    just          = just,
    hjust         = hjust,
    vjust         = vjust,
    interpolate   = interpolate,
    name          = name,
    gp            = gp,
    vp            = vp,
    default.units = default.units
  )
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                            #
#                            #
#     # ##    ###    ####  #####
#     ##     #   #  #        #
#     #      #####  #        #
#     #      #      #        #
#     #       ###    ####     ##
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname bbox_grob
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rect_grob <- function(
  x             =  0,
  y             =  0,
  width         = 20,
  height        = 20,
  just          = 'centre',
  hjust         = NULL,
  vjust         = NULL,
  default.units = getOption("ingrid.default.units", 'mm'),
  name          = NULL,
  gp            = NULL,
  vp            = NULL,
  centred       = TRUE,
  col, fill, alpha, lty, lwd, lex, lineend, linejoin, linemitre, fontsize, cex, fontfamily, fontface, lineheight,
  mask,clip,layout,layout.pos.row,layout.pos.col
) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create 'gp' and 'vp' if not given
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  args <- find_args()
  if (is.null(gp)) {
    gp <- do.call(ingrid::gp, args)
  }

  if (is.null(vp)) {
    vp_arg_names <- c('mask', 'clip', 'layout', 'layout.pos.row', 'layout.pos.col')
    vp_arg_names <- intersect(vp_arg_names, names(args))
    vp_args <- args[vp_arg_names]
    vp <- do.call(ingrid::vp, vp_args)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Standard unit handling
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x      <- make_unit(x     , default.units)
  y      <- make_unit(y     , default.units)
  width  <- make_unit(width , default.units)
  height <- make_unit(height, default.units)

  if (isTRUE(centred)) {
    x <- x + unit(0.5, 'npc')
    y <- y + unit(0.5, 'npc')
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the grob
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  grid::rectGrob(
    x      = x,
    y      = y,
    width  = width,
    height = height,
    just   = just,
    hjust  = hjust,
    vjust  = vjust,
    name   = name,
    gp     = gp,
    vp     = vp,
    default.units = default.units
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                     #                         #
#                                     #                         #
#     # ##    ###   #   #  # ##    ####  # ##    ###    ####  #####
#     ##     #   #  #   #  ##  #  #   #  ##     #   #  #        #
#     #      #   #  #   #  #   #  #   #  #      #####  #        #
#     #      #   #  #  ##  #   #  #   #  #      #      #        #
#     #       ###    ## #  #   #   ####  #       ###    ####     ##
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a grob
#'
#' @inheritParams bbox_grob
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
roundrect_grob <- function(
  x      =  0,
  y      =  0,
  width  = 20,
  height = 20,
  just   = c('left', 'bottom'),
  hjust  = NULL,
  vjust  = NULL,
  default.units = getOption("ingrid.default.units", 'mm'),
  name    = NULL,
  gp      = NULL,
  vp      = NULL,
  centred = TRUE,
  col, fill, alpha, lty, lwd, lex, lineend, linejoin, linemitre, fontsize, cex, fontfamily, fontface, lineheight,
  mask,clip,layout,layout.pos.row,layout.pos.col
) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create 'gp' and 'vp' if not given
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  args <- find_args()
  if (is.null(gp)) {
    gp <- do.call(ingrid::gp, args)
  }

  if (is.null(vp)) {
    vp_arg_names <- c('mask', 'clip', 'layout', 'layout.pos.row', 'layout.pos.col')
    vp_arg_names <- intersect(vp_arg_names, names(args))
    vp_args <- args[vp_arg_names]
    vp <- do.call(ingrid::vp, vp_args)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Standard unit handling
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x      <- make_unit(x     , default.units)
  y      <- make_unit(y     , default.units)
  width  <- make_unit(width , default.units)
  height <- make_unit(height, default.units)

  if (isTRUE(centred)) {
    x <- x + unit(0.5, 'npc')
    y <- y + unit(0.5, 'npc')
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the grob
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  grid::roundrectGrob(
    x      = x,
    y      = y,
    width  = width,
    height = height,
    just   = just,
    hjust  = hjust,
    vjust  = vjust,
    name   = name,
    gp     = gp,
    vp     = vp,
    default.units = default.units
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                                 #
#                                                 #
#      ####   ###    ####  ## #    ###   # ##   #####   ####
#     #      #   #  #   #  # # #  #   #  ##  #    #    #
#      ###   #####  #   #  # # #  #####  #   #    #     ###
#         #  #       ####  # # #  #      #   #    #        #
#     ####    ###       #  #   #   ###   #   #     ##  ####
#                    ###
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a grob
#'
#' @param x0,y0,x1,y1 Neveric vectors indicating the start and end of the
#'        segments
#'
#' @inheritParams bbox_grob
#' @inheritParams bezier_grob
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
segments_grob <- function(
  x0     =  0,
  y0     =  0,
  x1     = 20,
  y1     = 20,
  default.units = getOption("ingrid.default.units", 'mm'),
  arrow   = NULL,
  name    = NULL,
  gp      = NULL,
  vp      = NULL,
  centred = TRUE,
  col, fill, alpha, lty, lwd, lex, lineend, linejoin, linemitre, fontsize, cex, fontfamily, fontface, lineheight,
  mask,clip,layout,layout.pos.row,layout.pos.col
) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create 'gp' and 'vp' if not given
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  args <- find_args()
  if (is.null(gp)) {
    gp <- do.call(ingrid::gp, args)
  }

  if (is.null(vp)) {
    vp_arg_names <- c('mask', 'clip', 'layout', 'layout.pos.row', 'layout.pos.col')
    vp_arg_names <- intersect(vp_arg_names, names(args))
    vp_args <- args[vp_arg_names]
    vp <- do.call(ingrid::vp, vp_args)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Standard unit handling
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x <- make_unit(x, default.units)
  y <- make_unit(y, default.units)

  if (isTRUE(centred)) {
    x <- x + unit(0.5, 'npc')
    y <- y + unit(0.5, 'npc')
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the grob
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  grid::segmentsGrob(
    x0    = x0,
    x1    = x1,
    y0    = y0,
    y1    = y1,
    arrow = arrow,
    name  = name,
    gp    = gp,
    vp    = vp,
    default.units = default.units
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#       #                    #
#       #                    #
#     #####   ###   #   #  #####
#       #    #   #   # #     #
#       #    #####    #      #
#       #    #       # #     #
#        ##   ###   #   #     ##
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a grob
#'
#' @param rot The angle to rotate the text.
#' @param check.overlap A logical value to indicate whether to check for
#'        and omit overlapping text.
#' @param label A character or expression vector. Other objects are
#'        coerced by as.graphicsAnnot.
#'
#' @inheritParams bbox_grob
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
text_grob <- function(
  label         = "Hello #RStats",
  x             =  0,
  y             =  0,
  just          = c('left', 'bottom'),
  hjust         = NULL,
  vjust         = NULL,
  rot           = 0,
  check.overlap = FALSE,
  default.units = getOption("ingrid.default.units", 'mm'),
  name          = NULL,
  gp            = NULL,
  vp            = NULL,
  centred       = TRUE,
  col, fill, alpha, lty, lwd, lex, lineend, linejoin, linemitre, fontsize, cex, fontfamily, fontface, lineheight,
  mask,clip,layout,layout.pos.row,layout.pos.col
) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create 'gp' and 'vp' if not given
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  args <- find_args()
  if (is.null(gp)) {
    gp <- do.call(ingrid::gp, args)
  }

  if (is.null(vp)) {
    vp_arg_names <- c('mask', 'clip', 'layout', 'layout.pos.row', 'layout.pos.col')
    vp_arg_names <- intersect(vp_arg_names, names(args))
    vp_args <- args[vp_arg_names]
    vp <- do.call(ingrid::vp, vp_args)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Standard unit handling
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x <- make_unit(x, default.units)
  y <- make_unit(y, default.units)

  if (isTRUE(centred)) {
    x <- x + unit(0.5, 'npc')
    y <- y + unit(0.5, 'npc')
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the grob
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  grid::textGrob(
    label         = label,
    x             = x,
    y             = y,
    just          = just,
    hjust         = hjust,
    vjust         = vjust,
    rot           = rot,
    check.overlap = check.overlap,
    name          = name,
    gp            = gp,
    vp            = vp,
    default.units = default.units
  )
}






