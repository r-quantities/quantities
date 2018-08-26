#' Handle Measurement Uncertainty on a Numeric Vector
#'
#' Set or retrieve measurement uncertainty to/from numeric vectors (extensions
#' to the \pkg{errors} package for \code{quantities} and \code{units} objects).
#'
#' @inheritParams errors::errors
#' @inheritParams quantities
#'
#' @seealso \code{\link[errors]{errors}}.
#'
#' @name errors
#' @export
errors.quantities <- function(x) reclass(NextMethod())

#' @name errors
#' @export
errors.units <- function(x) {
  if (length(class(x)) > 1) return(NextMethod())
  utils::getS3method("errors", "numeric")(x)
}

#' @name errors
#' @export
errors.mixed_units <- function(x) sapply(x, errors)

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
`errors<-.mixed_units` <- function(x, value)
  structure(mapply(set_errors, x, value, SIMPLIFY=FALSE), class="mixed_units")

#' @name errors
#' @export
set_errors.quantities <- utils::getS3method("set_errors", "errors")

#' @name errors
#' @export
set_errors.units <- utils::getS3method("set_errors", "numeric")

#' @name errors
#' @export
set_errors.mixed_units <- utils::getS3method("set_errors", "numeric")

#' @name errors
#' @export
errors_max.quantities <- function(x) {
  set_units(NextMethod(), units(x), mode="standard")
}

#' @name errors
#' @export
errors_min.quantities <- errors_max.quantities
