#' Handle Measurement Units on a Numeric Vector
#'
#' Set or retrieve measurement units to/from numeric vectors and convert units
#' (extensions to the \pkg{units} package for \code{quantities} and
#' \code{errors} objects).
#'
#' @inheritParams units::units
#' @inheritParams units::set_units
#' @inheritParams units::mixed_units
#' @inheritParams quantities
#'
#' @seealso \code{\link[units]{units}}, \code{\link[units:units]{set_units}}.
#'
#' @name units
#' @export
`units<-.quantities` <- function(x, value) {
  e <- errors(x) * get_scaling(units(x), value)
  x <- NextMethod()
  errors(x) <- e
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
set_units.quantities <- getS3method("set_units", "units")

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
