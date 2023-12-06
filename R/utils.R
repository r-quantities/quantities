reclass <- function(x) {
  if (!is.null(attr(x, "units")) && !is.null(attr(x, "errors")))
    class(x) <- c("quantities", "units", "errors")
  x
}

dfapply <- function(X, FUN, ...) {
  attrs <- attributes(X)
  X <- lapply(X, FUN, ...)
  attributes(X) <- attrs
  X
}
