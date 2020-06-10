reclass <- function(x) {
  if (!is.null(attr(x, "units")) && !is.null(attr(x, "errors")))
    class(x) <- c("quantities", "units", "errors")
  x
}

is_offset <- function(prev, new) {
  offset <- 0
  units(offset) <- prev
  units(offset) <- new
  unclass(offset) != 0
}
