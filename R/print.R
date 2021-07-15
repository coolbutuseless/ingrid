


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' print a grob a bit more verbose
#'
#' @param x object to print
#' @param prefix Prefix for first line of output. Default: NULL
#' @param depth indentation depth. Default: 1L
#' @param include_gp,include_vp print the graphics parameters and viewport?
#'        default: TRUE for both
#' @param ... arguments which may be passed to other functions
#'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.grob <- function(x, depth = 1L, include_gp = TRUE, include_vp = TRUE, prefix = NULL, ...) {
  param_names <- NULL
  grob_type   <- NULL

  if (inherits(x, 'null')) {
    grob_type <- 'null'
  } else if (inherits(x, 'circle')) {
    grob_type <- 'circle'
    param_names <- c('x', 'y', 'r')
  } else if (inherits(x, 'rect')) {
    grob_type <- 'rect'
    param_names <- c('x', 'y', 'width', 'height', 'just', 'hjust', 'vjust')
  } else if (inherits(x, 'polygon')) {
    grob_type <- 'polygon'
    param_names <- c('x', 'y', 'id', 'id.lengths')
  } else if (inherits(x, 'polyline')) {
    grob_type <- 'polyline'
    param_names <- c('x', 'y', 'id', 'id.lengths', 'arrow')
  } else if (inherits(x, 'lines')) {
    grob_type <- 'lines'
    param_names <- c('x', 'y', 'arrow')
  } else if (inherits(x, 'roundrectGrob')) {
    grob_type <- 'roundrectGrob'
    param_names <- c('x', 'y', 'width', 'height', 'r')
  } else if (inherits(x, 'segments')) {
    grob_type <- 'segments'
    param_names <- c('x0', 'y0', 'x1', 'y1', 'arrow')
  } else if (inherits(x, 'pathgrob')) {
    grob_type <- 'pathgrob'
    param_names <- c('x', 'y', 'id', 'id.lengths', 'pathId', 'pathId.lengths', 'rule')
  } else if (inherits(x, 'xspline')) {
    grob_type <- 'xspline'
    param_names <- c('x', 'y', 'id', 'id.lengths', 'shape', 'open', 'arrow', 'repEnds')
  } else if (inherits(x, 'beziergrob')) {
    grob_type <- 'beziergrob'
    param_names <- c('x', 'y', 'id', 'id.lengths', 'arrow')
  } else if (inherits(x, 'rastergrob')) {
    grob_type <- 'rastergrob'
    param_names <- c('x', 'y', 'width', 'height', 'just', 'hjust', 'vjust',
                     'interpolate')
  } else if (inherits(x, 'text')) {
    grob_type <- 'text'
    param_names <- c('label', 'x', 'y', 'just', 'hjust', 'vjust', 'rot')
  } else if (inherits(x, 'points')) {
    grob_type <- 'points'
    param_names <- c('x', 'y', 'pch', 'size')
  } else if (inherits(x, 'clip')) {
    grob_type <- 'clip'
    param_names <- c('x', 'y', 'width', 'height', 'just', 'hjust', 'vjust')
  } else if (inherits(x, 'functiongrob')) {
    grob_type <- 'functiongrob'
    param_names <- c('range', 'n')
  } else if (inherits(x, 'frame')) {
    grob_type <- 'frame'
    param_names <- c()
  }

  cat0(depth, prefix, grob_type, " [", as.character(x$name), "]\n")

  if (inherits(x, 'rastergrob')) {
    cat0(depth + 1L, "raster: ", paste0(dim(x$raster), collapse = " x "), "\n")
  }
  if (!is.null(param_names)) {
    vals <- x[param_names]
    vals <- Filter(Negate(is.null), vals)
    cat0(depth + 1L, paste_args(vals), "\n")
  }
  if (include_gp) print.gpar    (x$gp, depth = depth + 1L)
  if (include_vp) print.viewport(x$vp, depth = depth + 1L)

  invisible(x)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' print a gpar a bit more verbose
#'
#' @inheritParams print.grob
#'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.gpar <- function(x, depth = 1L, prefix = NULL, ...) {
  cat0(depth, "gp: ")
  if (is_empty(x)) {
    cat("default\n")
    return(invisible(x))
  }

  cat("\n")

  nn <- names(x)
  for (n in nn) {
    val <- x[[n]]
    if (is_pattern(val)) {
      cat0(depth + 1L, n, ": pattern\n")
      print(val, depth = depth + 2L)
    } else if (is_grob(val)) {
      cat0(depth + 1L, n, ": grob\n")
      print(val, depth = depth + 2L)
    } else {
      cat0(depth + 1L, n, ": ", val, "\n")
    }
  }

  invisible(x)
}


default_viewport <- grid::viewport()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' print a gpar a bit more verbose
#'
#' @inheritParams print.grob
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.viewport <- function(x, depth = 1L, prefix = NULL, ...) {
  cat0(depth, prefix, "vp: [", as.character(x$name %||% "NULL"), "] ")
  if (is_empty(x)) {
    cat("default\n")
    return(invisible(x))
  }

  nn <- setdiff(names(default_viewport), 'name')
  match <- vapply(
    nn,
    function(n) {
      identical(default_viewport[[n]], x[[n]])
    },
    logical(1)
  )

  changed_names <- nn[!match]

  if (is.null(changed_names) || length(changed_names) == 0) {
    cat("default\n")
    return(invisible(x))
  }

  cat("\n")

  for (cname in changed_names) {
    val <- x[[cname]]
    if (is_grob(val)) {
      cat0(depth + 1L, cname, ": grob\n")
      print(val, depth = depth + 2L)
    } else if (inherits(val, 'GridMask')) {
      cat0(depth + 1L, cname, ": GridMask\n")
      print(val, depth = depth + 1L)
    } else {
      cat0(depth + 1L, cname, ": ", x[[cname]], "\n")
    }
  }

  invisible(x)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' print a grob a bit more verbose
#'
#' @inheritParams print.grob
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.polyclipgrob <- function(x, depth = 1L, prefix = NULL, ...) {

  cat0(depth, prefix, x$op, " [", as.character(x$name), "]\n")
  print(x$A, depth = depth + 1L, prefix = "A: ", include_gp = FALSE)
  print(x$B, depth = depth + 1L, prefix = "B: ", include_gp = FALSE)

  print.gpar    (x$gp, depth = depth + 1L)
  print.viewport(x$vp, depth = depth + 1L)

  invisible(x)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' print a grob a bit more verbose
#'
#' @inheritParams print.grob
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.gTree <- function(x, depth = 1L, prefix = NULL, ...) {

  cat0(depth, prefix, "[", as.character(x$name), "]\n")
  for (idx in seq_along(x$childrenOrder)) {
    child_name <- x$childrenOrder[idx]
    print(x$children[[child_name]], depth = depth + 1L, prefix = paste0("[", idx, "] "))
  }

  print.viewport(x$vp, depth = depth + 1L)

  invisible(x)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Character representation of a grid pattern object
#'
#' @inheritParams print.grob
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.GridPattern <- function(x, ...) {
  class(x)[[1]]
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Print a pattern
#'
#' @inheritParams print.grob
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.GridLinearGradient <- function(x, depth = 1L, prefix = NULL, ...) {
  cat0(depth, "LinearGradient\n")

  cat0(
    depth + 1L,
    "coords : ",
    sprintf("[%s, %s] - [%s, %s]\n", x$x1, x$y1, x$x2, x$y2)
  )

  cat0(
    depth + 1L,
    "colours: ",
    paste(x$stop, x$colours, sep=":", collapse = ", "),
    "\n"
  )

  cat0(depth + 1L, "Extend: ", x$extend, "\n")

  invisible(x)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname print.GridLinearGradient
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.GridRadialGradient <- function(x, depth = 1L, prefix = NULL, ...) {
  cat0(depth, prefix, "RadialGradient\n")

  cat0(
    depth + 1L,
    "coords : ",
    sprintf(
      "[%s, %s] r=%s - [%s, %s] r=%s\n",
      x$cx1, x$cy1, x$r1,
      x$cx2, x$cy2, x$r2
    )
  )

  cat0(
    depth + 1L,
    "colours: ",
    paste(x$stop, x$colours, sep=":", collapse = ", "),
    "\n"
  )

  cat0(depth + 1L, "Extend: ", x$extend, "\n")

  invisible(x)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname print.GridLinearGradient
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.GridTilingPattern <- function(x, depth = 1L, prefix = NULL, ...) {
  cat0(depth, prefix, "TilingPattern\n")
  cat0(
    depth + 1L,
    "coords : ",
    sprintf("[%s, %s] - [%s, %s]\n", x$x, x$y, x$width, x$height)
  )

  cat0(
    depth + 1L,
    sprintf("hjust: %f  vjust: %f\n", x$hjust, x$vjust)
  )

  cat0(depth + 1L, "Extend: ", x$extend, "\n")

  ee <- environment(x$f)
  print(ee$grob, depth = depth + 1L)

  invisible(x)
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname print.GridLinearGradient
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.GridMask <- function(x, depth = 1L, prefix = NULL, ...) {
  cat0(depth, prefix, "GridMask\n")

  ee <- environment(x$f)
  print(ee$mask, depth = depth + 1L)

  invisible(x)
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Register the verbose printing methods for grobs, polyclipgrobs, gTrres, gpars, and viewports
#'
#' These will all override the standard print methods in the \code{grid} package.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
register_verbose_printing <- function() {
  registerS3method(
    genname = "print",
    class   = "grob",
    method  = "print.grob",
    envir   = getNamespace("ingrid")
  )
  registerS3method(
    genname = "print",
    class   = "polyclipgrob",
    method  = "print.polyclipgrob",
    envir   = getNamespace("ingrid")
  )
  registerS3method(
    genname = "print",
    class   = "gTree",
    method  = "print.gTree",
    envir   = getNamespace("ingrid")
  )
  registerS3method(
    genname = "print",
    class   = "viewport",
    method  = "print.viewport",
    envir   = getNamespace("ingrid")
  )
  registerS3method(
    genname = "print",
    class   = "gpar",
    method  = "print.gpar",
    envir   = getNamespace("ingrid")
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Deregister the verbose printing methods
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
deregister_verbose_printing <- function() {
  registerS3method(
    genname = "print",
    class   = "grob",
    method  = "print.grob",
    envir   = getNamespace("grid")
  )
  registerS3method(
    genname = "print",
    class   = "polyclipgrob",
    method  = "print.grob",
    envir   = getNamespace("grid")
  )
  registerS3method(
    genname = "print",
    class   = "gTree",
    method  = "print.grob",
    envir   = getNamespace("grid")
  )
  registerS3method(
    genname = "print",
    class   = "viewport",
    method  = "print.viewport",
    envir   = getNamespace("grid")
  )
  registerS3method(
    genname = "print",
    class   = "gpar",
    method  = "print.gpar",
    envir   = getNamespace("grid")
  )
}






