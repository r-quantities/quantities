#' Handle Measurement Units on a Numeric Vector
#'
#' Set or retrieve measurement units to/from numeric vectors and convert units
#' (extensions to the \pkg{units} package for \code{quantities} and
#' \code{errors} objects).
#'
#' @param x a numeric object, or object of class \code{quantities}, \code{units}
#' or \code{errors}.
#' @inheritParams units::units
#' @inheritParams units::mixed_units
#'
#' @details For objects of class \code{quantities}, methods \code{`units<-`()}
#' and \code{set_units()} automatically convert the associated uncertainty to
#' the new unit (see examples below).
#'
#' @seealso \code{\link[units]{units}}, \code{\link[units:units]{set_units}}.
#'
#' @examples
#' (x <- set_quantities(1:5, m, 0.01))
#' set_units(x, cm)
#'
#' @name units
#' @export
`units<-.quantities` <- function(x, value) {
  xx <- errors_max(x)
  units(xx) <- value
  x <- unclass(NextMethod())
  e <- as.numeric(xx) - as.numeric(x)
  errors(x) <- e
  reclass(x)
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
set_units.errors <- getS3method("set_units", "units")

#' @name units
#' @export
mixed_units.quantities <- function(x, values, ...)
  set_errors(NextMethod(), errors(x))

#' @name units
#' @export
mixed_units.errors <- getS3method("mixed_units", "numeric")
