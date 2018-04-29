#' Parse Units and Errors
#'
#' Functions to parse character vectors into quantities.
#'
#' @param x a character vector to parse.
#' @return A \code{quantities}, \code{units} or \code{errors} object respectively.
#'
#' @details Each \code{parse_*()} function returns an object of the corresponding
#' type, no matter what it is found. This means that, for \code{parse_units}, if
#' errors are found, they are dropped with a warning. Similarly for
#' \code{parse_errors}, if units are found, they are dropped with a warning.
#' On the other hand, \code{parse_quantities} always returns a valid
#' \code{quantities} object, even if no errors or units are found (then, zero
#' error and dimensionless units are applied).
#'
#' @examples
#' parse_quantities("(1.6021766208 +/- .0000000098) e-19 C")
#' parse_quantities("1.6021766208(98) e-19 C")
#' parse_units("1.6021766208 e-19 C")
#' parse_errors("1.6021766208(98) e-19")
#'
#' @importFrom Rcpp evalCpp
#' @useDynLib quantities, .registration=TRUE
#' @export
parse_quantities <- function(x) {
  x <- parse_quantities_(x)

  if (all(attr(x, "units") == attr(x, "units")[1])) {
    units(x) <- attr(x, "units")[1]
    return(reclass(x))
  }

  do.call(c, lapply(seq_along(x), function(i) {
    set_quantities(x[i], attr(x, "units")[i], attr(x, "errors")[i], mode="standard")
  }))
}

#' @rdname parse_quantities
#' @export
parse_units <- function(x) {
  x <- parse_quantities_(x)

  if (any(attr(x, "errors") != 0))
    warning("errors present but ignored")
  attr(x, "errors") <- NULL

  if (all(attr(x, "units") == attr(x, "units")[1]))
    return(set_units(x, attr(x, "units")[1], mode="standard"))

  do.call(c, lapply(seq_along(x), function(i) {
    set_units(x[i], attr(x, "units")[i], mode="standard")
  }))
}

#' @rdname parse_quantities
#' @export
parse_errors <- function(x) {
  x <- parse_quantities_(x)

  if (any(attr(x, "units") != "1"))
    warning("units present but ignored")
  attr(x, "units") <- NULL

  class(x) <- "errors"
  x
}
