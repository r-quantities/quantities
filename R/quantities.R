#' \pkg{quantities}: Quantity Calculus for R Vectors
#'
#' Support for painless automatic units and uncertainty propagation in numerical
#' operations. Both units and errors are integrated into a complete quantity
#' calculus system within the R language. R vectores, matrices and arrays
#' automatically propagate those attributes when you operate with \code{quantities}
#' objects.
#'
#' @author Iñaki Ucar
#'
#' @docType package
#' @import units
#' @import errors
#' @name quantities-package
NULL

#' Set Measurement Units and Errors on a Numeric Vector
#'
#' Set/retrieve measurement units and errors to/from numeric vectors.
#'
#' @param x a numeric object, or object of class \code{quantities}, \code{units}
#' or \code{errors}.
#'
#' @details \code{quantities} returns a named list with the \code{units} and
#' \code{errors} attributes.
#'
#' \code{`quantities<-`} sets the units and error values (and converts \code{x}
#' into an object of class \code{quantities}). \code{set_quantities} is a
#' pipe-friendly version of \code{`quantities<-`} and returns an object of class
#' \code{quantities}.
#'
#' @seealso
#' \code{\link{errors}}, \code{\link{units}}, \code{\link{groupGeneric.quantities}}.
#' \code{\link{Extract.quantities}}, \code{\link{c}}, \code{\link{rep}}, \code{\link{cbind.quantities}}.
#' \code{\link{as.data.frame.quantities}}, \code{\link{as.matrix.quantities}}, \code{\link{t}}.
#'
#' @examples
#' x = 1:3
#' class(x)
#' x
#' quantities(x) <- list("m/s", 0.1)
#' class(x)
#' x
#'
#' (x <- set_quantities(x, m/s, seq(0.1, 0.3, 0.1)))
#'
#' @export
quantities <- function(x) UseMethod("quantities")

#' @export
quantities.quantities <- function(x) {
  list(units=attr(x, "units"), errors=attr(x, "errors"))
}

#' @name quantities
#' @param value a list of two components: an object of class \code{units} or
#' \code{symbolic_units} (see \code{\link[units]{units}}), and a numeric vector
#' of length 1 or the same length as \code{x} (see \code{\link[errors]{errors}}).
#' @export
`quantities<-` <- function(x, value) UseMethod("quantities<-")

#' @export
`quantities<-.quantities` <- function(x, value) {
  if (is.null(value)) return(drop_quantities(x))
  stopifnot(length(value) == 2)

  units(x) <- value[[1]]
  errors(x) <- value[[2]]
  x
}

#' @export
`quantities<-.numeric` <- function(x, value) {
  if (is.null(value)) return(x)
  `quantities<-.quantities`(x, value)
}

#' @export
`quantities<-.units` <- function(x, value) {
  if (is.null(value)) return(drop_units(x))
  `quantities<-.quantities`(x, value)
}

#' @export
`quantities<-.errors` <- function(x, value) {
  if (is.null(value)) return(drop_errors(x))
  `quantities<-.quantities`(x, value)
}

#' @name quantities
#' @param unit a \code{units} object, or something coercible to one with
#' \code{as_units} (see \code{\link[units]{set_units}}).
#' @param errors a numeric vector of length 1 or the same length as \code{x}
#' (see \code{\link[errors:errors]{set_errors}}).
#' @export
set_quantities <- function(x, unit, errors=0) UseMethod("set_quantities")

#' @export
set_quantities.numeric <- function(x, unit, errors=0) {
  if (missing(unit))
    unit <- unitless
  else unit <- substitute(unit)
  quantities(x) <- list(as_units(unit), errors)
  x
}

#' @export
set_quantities.quantities <- set_quantities.numeric

#' @export
set_quantities.units <- set_quantities.numeric

#' @export
set_quantities.errors <- set_quantities.numeric

#' Set Measurement Errors on a Numeric Vector
#'
#' Set/retrieve measurement errors to/from numeric vectors (extensions to the
#' \code{errors} package for \code{quantities} and \code{units} objects).
#'
#' @inheritParams errors::errors
#' @inheritParams quantities
#'
#' @seealso \code{\link[errors]{errors}}.
#'
#' @name errors
#' @export
`errors<-.quantities` <- function(x, value) reclass(NextMethod())

#' @name errors
#' @export
`errors<-.units` <- function(x, value) {
  if (length(class(x)) > 1) return(NextMethod())

  x <- unclass(x)
  errors(x) <- value
  class(x) <- "units"
  reclass(x)
}

#' @name errors
#' @export
set_errors.quantities <- function(x, value=0) reclass(NextMethod())

#' @name errors
#' @export
set_errors.units <- getS3method("set_errors", "errors")

#' @name errors
#' @export
errors_max.quantities <- function(x) {
  set_units(NextMethod(), units(x), mode="standard")
}

#' @name errors
#' @export
errors_min.quantities <- errors_max.quantities

#' Set Measurement Units on a Numeric Vector
#'
#' Set/retrieve measurement units to/from numeric vectors and convert units
#' (extensions to the \code{units} package for \code{quantities} and
#' \code{errors} objects).
#'
#' @inheritParams units::units
#' @inheritParams units::set_units
#' @inheritParams quantities
#'
#' @seealso \code{\link[units]{units}}, \code{\link[units]{set_units}}.
#'
#' @name units
#' @export
`units<-.quantities` <- function(x, value) {
  prev <- unclass(x[[1]])
  x <- reclass(NextMethod())
  errors(x) <- errors(x) * unclass(x[[1]]) / prev
  x
}

#' @name units
#' @export
`units<-.errors` <- function(x, value) {
  x <- unclass(x)
  units(x) <- value
  class(x) <- "errors"
  reclass(x)
}

#' @name units
#' @export
set_units.quantities <- function(x, value, ...) {
  if (missing(value))
    value <- unitless
  else value <- substitute(value)
  units(x) <- value
  x
}

#' @name units
#' @export
set_units.errors <- function(x, value, ...)
  getS3method("set_units", "units")(x, value, ...)

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
