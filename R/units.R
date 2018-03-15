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
set_units.quantities <- utils::getS3method("set_units", "units")

#' @name units
#' @export
set_units.errors <- utils::getS3method("set_units", "units")
