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
