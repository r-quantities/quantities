#' @rdname groupGeneric.quantities
#'
#' @examples
#' c(min(x), max(x))
#' range(x)
#' sum(y)
#'
#' @export
Summary.quantities <- function(..., na.rm = FALSE) reclass(NextMethod())

#' @export
mean.quantities <- function(x, ...) reclass(NextMethod())

#' @export
weighted.mean.quantities <- mean.quantities

#' @export
median.quantities <- mean.quantities
