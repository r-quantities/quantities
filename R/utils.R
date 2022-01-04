reclass <- function(x) {
  if (!is.null(attr(x, "units")) && !is.null(attr(x, "errors")))
    class(x) <- c("quantities", "units", "errors")
  x
}
