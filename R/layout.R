
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Functions for laying out multiple grobs
#'
#' @param ... grobs
#' @param widths,heights the relative widths or heights of the grobs for the
#'        \code{row()} and and \code{col()} layouts respectively.  Default: NULL
#'        means to allocate equal space to all grobs
#'
#' @return a grob object combining the given grobs
#'
#' @import grid
#' @export
#'
#' @family layout
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
igl_row <- function(..., widths = NULL) {
  grobs <- list(...)
  N     <- length(grobs)
  stopifnot(N > 0)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanitize widths
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(widths)) {
    widths <- rep_len(1, N)
  }

  if (length(widths) != N) {
    stop("igl_row(): length(widths) does not match number of grobs")
  }

  if (!grid::is.unit(widths)) {
    widths <- unit(widths, 'null')
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a frame with the given layout
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  layout <- grid::grid.layout(nrow = 1, ncol = N, widths = widths)
  frame  <- grid::frameGrob(layout = layout)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Place the given grobs in this frame
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (i in seq(N)) {
    frame <- placeGrob(frame, grobs[[i]], col = i)
  }

  frame
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname igl_row
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
igl_col <- function(..., heights = NULL) {
  grobs <- list(...)
  N     <- length(grobs)
  stopifnot(N > 0)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanitize heights
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(heights)) {
    heights <- rep_len(1, N)
  }

  if (length(heights) != N) {
    stop("igl_col(): length(heights) does not match number of grobs")
  }

  if (!grid::is.unit(heights)) {
    heights <- unit(heights, 'null')
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a frame with the given layout
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  layout <- grid::grid.layout(ncol = 1, nrow = N, heights = heights)
  frame  <- grid::frameGrob(layout = layout)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Place the given grobs in this frame
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (i in seq(N)) {
    frame <- placeGrob(frame, grobs[[i]], row = i)
  }

  frame
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Nested layout with an arbitrary \code{grid::viewport()}
#'
#' @param x,y,width,height,default.units,just,gp,clip,mask,xscale,yscale,angle,layout,layout.pos.row,layout.pos.col,name
#'        See \code{grid::viewport()} documentation.
#' @param ... grobs within this viewport
#'
#' @import grid
#' @export
#'
#' @family layout
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
igl_vp <- function(...,
                   x              = unit(0.5, "npc"),
                   y              = unit(0.5, "npc"),
                   width          = unit(1, "npc"),
                   height         = unit(1, "npc"),
                   default.units  = "npc",
                   just           = "centre",
                   gp             = gpar(),
                   clip           = "inherit",
                   mask           = "inherit",
                   xscale         = c(0, 1),
                   yscale         = c(0, 1),
                   angle          = 0,
                   layout         = NULL,
                   layout.pos.row = NULL,
                   layout.pos.col = NULL,
                   name           = NULL) {

  this_vp <- grid::viewport(
    x              = x,
    y              = y,
    width          = width,
    height         = height,
    default.units  = default.units,
    just           = just,
    gp             = gp,
    clip           = clip,
    mask           = mask,
    xscale         = xscale,
    yscale         = yscale,
    angle          = angle,
    layout         = layout,
    layout.pos.row = layout.pos.row,
    layout.pos.col = layout.pos.col,
    name           = name
  )

  grobTree(..., vp = this_vp)
}


if (FALSE) {
  frame <- igl_row(
    igl_col(
      rectGrob(width = 0.5, height = 0.5, gp = gpar(fill = 'red'  ), default.units = 'snpc'),
      rectGrob(width = 0.5, height = 0.5, gp = gpar(fill = 'green'), default.units = 'snpc')
    ),
    igl_col(
      rectGrob(width = 0.5, height = 0.5, gp = gpar(fill = 'blue' ), default.units = 'snpc'),
      igl_vp(
        angle = 45,
        rectGrob(gp = gpar(fill = NA, col = 'black')),
        igl_row(
          rectGrob(width = 0.5, height = 0.5, gp = gpar(fill = 'hotpink'), default.units = 'snpc'),
          rectGrob(width = 0.5, height = 0.5, gp = gpar(fill = 'yellow' ), default.units = 'snpc')
        )
      )
    )
  )

  grid::grid.newpage()
  grid::grid.draw(frame)
}


