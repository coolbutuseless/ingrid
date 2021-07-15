

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Thing wrapper around \code{grid::viewport()}
#'
#' @param x,y,width,height,default.units,just,gp,clip,mask,xscale,yscale,angle,layout,layout.pos.row,layout.pos.col,name
#'        See \code{grid::viewport()} documentation.
#' @param ... other arguments ignored
#'
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vp <- function(
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
  name           = NULL,
  ...
) {

  grid::viewport(
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
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a 'grob' mask into a 'GridMask' object for inclusion in a viewport object
#'
#' @param mask grob
#'
#' @return 'GridMask' object
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_mask <- function (mask) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Include the mask in the body of this 'create_mask' function
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  force(mask)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # The function for rendering the mask will then include the current
  # environment - including the mask object itself
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  maskFun <- function() {
    grid.draw(mask, recording = FALSE)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # The maskFun (and hence the environment containing the mask) is wrapped
  # as a simple list object with a 'GridMask' clas
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  result <- list(f = maskFun, ref = NULL)
  class(result) <- "GridMask"

  result
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Set a mask on an existing viewport
#'
#' @param vp viewport
#' @param mask One of "none" (or FALSE) or "inherit" (or TRUE) or a grob
#'        (or a gTree). This specifies that the viewport should have no mask,
#'        or it should inherit the mask of its parent, or it should have its
#'        own mask, as described by the grob.
#'
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vp_setmask <- function(vp, mask) {
  if (!is.logical(mask)) {
    if (is.grob(mask)) {
      mask <- create_mask(mask)
    }
    else {
      mask <- switch(
        as.character(mask),
        inherit = TRUE,
        none    = FALSE,
        stop("invalid 'mask' value: ", mask)
      )
    }
  }

  vp$mask <- mask

  vp
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a 'grob' clipping path into a 'GridClipPath' object for inclusion in a viewport object
#'
#' @param clip grob
#'
#' @return 'GridClipPath' object
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_clippath <- function(clip) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Include the clip in the body of this 'create_clippath' function
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  force(clip)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # The function for rendering the clip will then include the current
  # environment - including the clip object itself
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pathFun <- function() {
    grid.draw(clip, recording = FALSE)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # The pathFun (and hence the environment containing the clip) is wrapped
  # as a simple list object with a 'GridClipPath' clas
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  path <- list(f = pathFun, ref = NULL)
  class(path) <- "GridClipPath"

  path
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Set a clip on an existing viewport
#'
#' @param vp viewport
#' @param clip One of "on", "inherit", or "off", indicating whether to clip
#'  to the extent of this viewport, inherit the clipping region from the
#'  parent viewport, or turn clipping off altogether. For back-compatibility,
#'   a logical value of TRUE corresponds to "on" and FALSE corresponds to
#'   "inherit". May also be a grob (or a gTree) that describes a clipping path.
#'
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vp_setclip <- function(vp, clip) {
  if (!is.logical(clip)) {
    if (is.grob(clip)) {
      clip <- create_clippath(clip)
    }
    else {
      clip <- switch(
        as.character(clip),
        on      = TRUE,
        off     = NA,
        inherit = FALSE,
        stop("invalid 'clip' value: ", clip)
      )
    }
  }
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Rotate a viewport
#'
#' @param vp viewport
#' @param angle rotation angle in degrees
#'
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vp_rotate <- function(vp, angle) {

  if (is.null(vp)) {
    vp <- grid::viewport(angle = angle)
  } else {
    vp$angle <- vp$angle + angle
  }

  vp
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Translate a viewport
#'
#' @param vp viewport
#' @param x,y translation
#' @param default.units 'npc'
#'
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vp_translate <- function(vp, x, y, default.units = 'npc') {

  x <- make_unit(x, default.units)
  y <- make_unit(y, default.units)

  if (is.null(vp)) {
    vp <- grid::viewport()
  }

  vp$x <- vp$x + x
  vp$y <- vp$y + y

  vp
}



