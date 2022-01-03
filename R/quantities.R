#' \pkg{quantities}: Quantity Calculus for R Vectors
#'
#' Support for painless automatic units and uncertainty propagation in numerical
#' operations. Both \pkg{units} and \pkg{errors} are integrated into a complete
#' quantity calculus system within the R language. R vectors, matrices and arrays
#' automatically propagate those attributes when you operate with \code{quantities}
#' objects.
#'
#' @author Iñaki Ucar
#'
#' @references Edzer Pebesma, Thomas Mailund and James Hiebert (2016).
#' Measurement Units in \R. \emph{The R Journal}, 8(2), 486--494.
#' \doi{10.32614/RJ-2016-061}
#'
#' Iñaki Ucar, Edzer Pebesma and Arturo Azcorra (2018).
#' Measurement Errors in \R. \emph{The R Journal}, 10(2), 549--557.
#' \doi{10.32614/RJ-2018-075}
#'
#' @docType package
#' @import units
#' @import errors
#' @import stats
#' @import utils
#' @name quantities-package
NULL

#' Handle Measurement Units and Uncertainty on a Numeric Vector
#'
#' Set or retrieve measurement units and uncertainty to/from numeric vectors.
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
#' \code{\link{Extract.quantities}}, \code{\link{c.quantities}},
#' \code{\link{rep.quantities}}, \code{\link{cbind.quantities}}.
#' \code{\link{as.data.frame.quantities}}, \code{\link{as.matrix.quantities}},
#' \code{\link{t.quantities}}.
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
  if (!is.null(value[[2]]))
    units(value[[2]]) <- value[[1]]
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
#' \code{as_units} (see \code{\link[units:units]{set_units}}).
#' @param errors a numeric vector of length 1 or the same length as \code{x}
#' (see \code{\link[errors:errors]{set_errors}}).
#' @inheritParams units::set_units
#' @export
set_quantities <- function(x, unit, errors=0, ..., mode=units_options("set_units_mode"))
  UseMethod("set_quantities")

#' @export
set_quantities.numeric <- function(x, unit, errors=0, ..., mode=units_options("set_units_mode")) {
  if (missing(unit))
    unit <- unitless
  else if (mode == "symbols")
    unit <- substitute(unit)
  quantities(x) <- list(as_units(unit), errors)
  x
}

#' @export
set_quantities.quantities <- set_quantities.numeric

#' @export
set_quantities.units <- set_quantities.numeric

#' @export
set_quantities.errors <- set_quantities.numeric
