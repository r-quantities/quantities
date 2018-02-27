#' Drop Units and Errors
#'
#' @param x a \code{quantities} object.
#'
#' @return the numeric without any \code{units} or \code{errors} attributes,
#' while preserving other attributes like dimensions or other classes.
#'
#' @details \code{drop_quantities} is equivalent to \code{quantities(x) <- NULL}
#' or \code{set_quantities(x, NULL, NULL)}. \code{drop_units} is equivalent to
#' \code{units(x) <- NULL} or \code{set_units(x, NULL)}. \code{drop_errors} is
#' equivalent to \code{errors(x) <- NULL} or \code{set_errors(x, NULL)}.
#'
#' @export
drop_quantities <- function(x) UseMethod("drop_quantities")

#' @export
drop_quantities.quantities <- function(x) drop_errors(drop_units(x))

#' @name drop_quantities
#' @export
drop_units.quantities <- function(x) {
  class(x) <- setdiff(class(x), "quantities")
  NextMethod()
}

#' @name drop_quantities
#' @export
drop_errors.quantities <- drop_units.quantities
