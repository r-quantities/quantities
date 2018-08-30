#' Parse Units and Errors
#'
#' Functions to parse character vectors into quantities.
#'
#' @param x a character vector to parse.
#' @param decimal_mark the dot (\code{.}) if not provided.
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
#' # quantities are converted to the first unit
#' parse_quantities(c("12.34(2) m/s", "36.5(1) km/h"))
#'
#' # or kept as a list of mixed units
#' parse_quantities(c("1.02(5) g", "2.51(0.01) V", "(3.23 +/- 0.12) m"))
#'
#' @importFrom Rcpp evalCpp
#' @useDynLib quantities, .registration=TRUE
#' @export
parse_quantities <- function(x, decimal_mark) {
  x <- parse_vector(x, decimal_mark)

  if (all(attr(x, "units") == attr(x, "units")[1])) {
    units(x) <- attr(x, "units")[1]
    return(reclass(x))
  }

  do.call(c, c(lapply(seq_along(x), function(i) {
    set_quantities(x[i], attr(x, "units")[i], attr(x, "errors")[i], mode="standard")
  }), allow_mixed=TRUE))
}

#' @rdname parse_quantities
#' @export
parse_units <- function(x, decimal_mark) {
  x <- parse_vector(x, decimal_mark)

  if (any(attr(x, "errors") != 0))
    warning("errors present but ignored")
  attr(x, "errors") <- NULL

  if (all(attr(x, "units") == attr(x, "units")[1]))
    return(set_units(x, attr(x, "units")[1], mode="standard"))

  do.call(c, c(lapply(seq_along(x), function(i) {
    set_units(x[i], attr(x, "units")[i], mode="standard")
  }), allow_mixed = TRUE))
}

#' @rdname parse_quantities
#' @export
parse_errors <- function(x, decimal_mark) {
  x <- parse_vector(x, decimal_mark)

  if (any(attr(x, "units") != "1"))
    warning("units present but ignored")
  attr(x, "units") <- NULL

  class(x) <- "errors"
  x
}

parse_vector <- function(x, decimal_mark) {
  if (missing(decimal_mark)) decimal_mark = "."
  stopifnot(decimal_mark %in% c(".", ","))
  grouping_mark <- if(decimal_mark == ".") "," else "."

  parse_vector_(x, decimal_mark, grouping_mark)
}
