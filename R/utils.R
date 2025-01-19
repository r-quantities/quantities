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

.deparse <- function(dots, symarg, deparse.level) {
  deparse.level <- as.integer(deparse.level)
  if (identical(deparse.level, -1L)) deparse.level <- 0L # R Core's hack
  stopifnot(0 <= deparse.level, deparse.level <= 2)

  nm <- c( ## 0:
    function(i) NULL,
    ## 1:
    function(i) if(is.symbol(s <- symarg[[i]])) deparse(s) else NULL,
    ## 2:
    function(i) deparse(symarg[[i]])[[1L]])[[ 1L + deparse.level ]]
  Nms <- function(i) { if(!is.null(s <- names(symarg)[i]) && nzchar(s)) s else nm(i) }

  symarg <- as.list(symarg)[-1L]
  dnames <- sapply(seq_along(dots), Nms)
  if (!all(sapply(dnames, is.null)))
    names(dots) <- dnames
  dots
}
