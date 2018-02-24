#' @rdname groupGeneric.quantities
#'
#' @examples
#' c(min(x), max(x))
#' range(x)
#' sum(y)
#'
#' @export
Summary.quantities <- function(..., na.rm = FALSE) {
  x <- reclass(NextMethod())
  if (!("errors" %in% names(attributes(x))))
    class(x) <- "units"
  x
}

#' @export
mean.quantities <- function(x, ...) reclass(NextMethod())

#' @export
weighted.mean.quantities <- mean.quantities

#' @export
median.quantities <- mean.quantities
