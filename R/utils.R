reclass <- function(x) {
  class(x) <- c("quantities", "units", "errors")
  x
}
