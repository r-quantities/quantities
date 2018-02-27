#' S3 Group Generic Functions
#'
#' \code{Math}, \code{Ops} and \code{Summary} group generic methods for
#' \code{quantities} objects (see \code{\link[base]{groupGeneric}} for a
#' comprehensive list of available methods).
#'
#' @inheritParams base::groupGeneric
#' @name groupGeneric.quantities
#'
#' @details See \code{\link[errors]{groupGeneric.errors}},
#' \code{\link[units]{Ops.units}}, \code{\link[units]{Math.units}}, for further details.
#'
#' @examples
#' x <- set_quantities(1:3, m/s, 0.1)
#' exp(x)
#' log(x)
#' cumsum(x)
#' cumprod(x)
#'
#' @export
Math.quantities <- function(x, ...) {
  if (.Generic == "log1p")
    class(x) <- setdiff(class(x), "quantities")
  reclass(NextMethod())
}

#' @export
#' @method log10 quantities
log10.quantities <- function(x) log(x, 10)

#' @export
#' @method log2 quantities
log2.quantities <- function(x) log(x, 2)
