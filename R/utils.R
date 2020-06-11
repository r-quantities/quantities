reclass <- function(x) {
  if (!is.null(attr(x, "units")) && !is.null(attr(x, "errors")))
    class(x) <- c("quantities", "units", "errors")
  x
}

get_scaling <- function(prev, new) {
  x <- c(0, 1)
  units(x) <- prev
  units(x) <- new
  as.numeric(diff(x))
}
