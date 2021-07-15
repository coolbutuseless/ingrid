
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Idea borrowed from \code{ggplot2::theme()}
#'
#' Use of this function negates having to define default values for all the
#' arguments in the parent function
#'
#' @param ... all arguments from call to parent function
#'
#' @return list with only arguments used in the call
#'
#' @importFrom utils modifyList
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
find_args <- function (...) {
  env <- parent.frame()
  args <- names(formals(sys.function(sys.parent(1))))
  vals <- mget(args, envir = env)
  vals <- vals[!vapply(vals, function(x) {identical(x, quote(expr=))}, logical(1))]
  modifyList(vals, list(..., ... = NULL))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert an object into a single string
#'
#' @param x object
#' @param limit limit the length
#'
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_character <- function(x, limit = 40) {
  if (is.null(x)) {
    res <- "NULL"
  } else if (any(is.unit(x))) {
    res <- as.character(x)
    res <- paste0("c(", paste(res, collapse = ", "), ")")
  } else {
    res <- deparse1(x)
  }

  # Trim chars a bit if they're too long
  if (nchar(res) > limit) {
    res <- paste0(
      substr(res, 0, limit-4),
      "... "
    )
  }

  res
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Collapse a list of args into a single string representation.
#'
#' This is like "dput" but a bit cleaner
#'
#' @param args named list of arguments
#'
#' @return single character string
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
paste_args <- function(args) {

  nn <- names(args)
  vals <- vapply(args, as_character, character(1))

  paste(nn, vals, sep='=', collapse = ", ")
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Is a list or grob empty?
#' @param x object
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_empty <- function(x) {
  length(x) == 0
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Output spaces to terminal
#' @param x number of spaces
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
indent <- function(x) {
  cat(rep(' ', (x - 1) * 2), sep='')
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' concatenate and print
#'
#' @param depth indentation depth
#' @param ... objects to ptrint
#' @param sep sep. default blank.
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cat0 <- function(depth, ..., sep="") {
  cat(rep(' ', (depth - 1) * 2), sep='')
  cat(..., sep = sep)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Make a value into a unit if it isn't already
#'
#' @param val value
#' @param units units
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
make_unit <- function(val, units) {
  if (grid::is.unit(val)) {
    val
  } else {
    grid::unit(val, units = units)
  }
}



'%||%' <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}


