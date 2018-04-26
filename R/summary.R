#' @rdname groupGeneric.quantities
#'
#' @examples
#' c(min(x), max(x))
#' range(x)
#' sum(x)
#'
#' @export
Summary.quantities <- function(..., na.rm = FALSE) reclass(NextMethod())

#' @export
mean.quantities <-function(x, ...) reclass(NextMethod())

#' @export
weighted.mean.quantities <- function(x, ...) {
  u <- units(x)
  x <- weighted.mean(drop_units(x), ...)
  units(x) <- u
  x
}

#' @export
median.quantities <- mean.quantities
