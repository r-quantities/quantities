#' Set Measurement Units on a Numeric Vector
#'
#' Set/retrieve measurement units to/from numeric vectors and convert units
#' (extensions to the \code{units} package for \code{quantities} and
#' \code{errors} objects).
#'
#' @inheritParams units::units
#' @inheritParams units::set_units
#' @inheritParams units::mixed_units
#' @inheritParams quantities
#'
#' @seealso \code{\link[units]{units}}, \code{\link[units]{set_units}}.
#'
#' @name units
#' @export
`units<-.quantities` <- function(x, value) {
  e <- errors(x)
  if (!is_offset(units(x), value)) { # not offset, then scale
    units(e) <- units(x)
    units(e) <- value
    units(e) <- NULL
  }
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
set_units.quantities <- utils::getS3method("set_units", "units")

#' @name units
#' @export
set_units.errors <- utils::getS3method("set_units", "units")

#' @name units
#' @export
mixed_units.quantities <- function(x, values, ...)
  set_errors(NextMethod(), errors(x))

#' @name units
#' @export
mixed_units.errors <- utils::getS3method("mixed_units", "numeric")
