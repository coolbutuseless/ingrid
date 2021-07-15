
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Testing for grob types
#'
#' @param x object to test
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_vanilla_grob <- function(x) {
  inherits(x, 'grob') && !inherits(x, 'gTree') && !inherits(x, 'polyclipgrob')
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname is_vanilla_grob
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_polyclipgrob <- function(x) {
  inherits(x, 'polyclipgrob')
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname is_vanilla_grob
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_vanilla_gTree <- function(x) {
  inherits(x, 'gTree') && !inherits(x, 'polyclipgrob')
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname is_vanilla_grob
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_pattern <- function(x) {
  inherits(x, 'GridPattern')
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname is_vanilla_grob
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_grob <- function(x) {
  inherits(x, 'grob')
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname is_vanilla_grob
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_viewport <- function(x) {
  inherits(x, 'viewport')
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Generate a new name for a grob
#'
#' param grob grob
#' param type name of the type of grob. defaults to first class name
#' param force create and assign new name regardless of existing name? default: TRUE
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
init_grob_auto_name <- function() {
  grob_index <- 0L
  function(grob, type = NULL, force=TRUE) {
    if (is.null(grob$name) || isTRUE(force))  {
      grob_index <<- grob_index + 1L
      if (is.null(type)) {
        type <- class(grob)[[1L]]
      }
      grob$name <- paste("IN.GRID", type, grob_index, sep = '.')
    } else {
      grob$name <- as.character(grob$name)
    }
    grob
  }
}

grob_auto_name <- init_grob_auto_name()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Copy a grob to a new named object
#'
#' TODO this should probably recursiely descend through gTrees and polyclipgrobs
#' and rename all child objects as well.
#'
#' @param grob grob
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
grob_copy <- function(grob) {
  grob_auto_name(grob)
}



