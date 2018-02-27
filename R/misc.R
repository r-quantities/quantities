#' Extract or Replace Parts of an Object
#'
#' S3 operators to extract or replace parts of \code{quantities} objects.
#'
#' @inheritParams base::Extract
#' @param ... additional arguments to be passed to base methods
#' (see \code{\link[base]{Extract}}).
#' @name Extract.quantities
#'
#' @examples
#' x <- set_quantities(1:3, m/s, 0.1)
#' y <- set_quantities(4:6, m/s, 0.2)
#' (z <- rbind(x, y))
#' z[2, 2]
#' z[2, 2] <- -1
#' errors(z[[1, 2]]) <- 0.8
#' z[, 2]
#'
#' @export
`[.quantities` <- function(x, ...) reclass(NextMethod())

#' @rdname Extract.quantities
#' @export
`[[.quantities` <- `[.quantities`

#' @rdname Extract.quantities
#' @export
`[<-.quantities` <- function(x, ..., value) reclass(NextMethod())

#' @rdname Extract.quantities
#' @export
`[[<-.quantities` <- `[<-.quantities`

#' Replicate Elements of Vectors and Lists
#'
#' S3 method for \code{quantities} objects (see \code{\link{rep}}).
#'
#' @inheritParams base::rep
#'
#' @examples
#' rep(set_quantities(1, m/s, 0.1), 4)
#'
#' @export
rep.quantities <- function(x, ...) reclass(NextMethod())

#' Combine Values into a Vector or List
#'
#' S3 method for \code{quantities} objects (see \code{\link{c}}).
#'
#' @inheritParams base::c
#'
#' @examples
#' c(set_quantities(1, m/s, 0.2), set_quantities(30, km/h, 0.1))
#'
#' @export
c.quantities <- function(...) reclass(NextMethod())

#' Lagged Differences
#'
#' S3 method for \code{quantities} objects (see \code{\link{diff}}).
#'
#' @inheritParams base::diff
#'
#' @examples
#' diff(set_quantities(1:10, m/s, 0.1), 2)
#' diff(set_quantities(1:10, m/s, 0.1), 2, 2)
#' x <- cumsum(cumsum(set_quantities(1:10, m/s, 0.1)))
#' diff(x, lag = 2)
#' diff(x, differences = 2)
#'
#' @export
diff.quantities <- function(x, lag = 1L, differences = 1L, ...) reclass(NextMethod())

#' Coerce to a Data Frame
#'
#' S3 method for \code{quantities} objects (see \code{\link{as.data.frame}}).
#'
#' @inheritParams base::as.data.frame
#'
#' @examples
#' x <- set_quantities(1:3, m/s, 0.1)
#' y <- set_quantities(4:6, m/s, 0.2)
#' (z <- cbind(x, y))
#' as.data.frame(z)
#'
#' @export
as.data.frame.quantities <- function(x, row.names = NULL, optional = FALSE, ...) {
  e <- errors(x)
  dim(e) <- dim(x)
  e <- as.data.frame(e)
  value <- NextMethod()
  if (!optional && ncol(value) == 1)
    colnames(value) <- deparse(substitute(x))
  for (i in seq_len(ncol(value))) {
    value[[i]] <- reclass(value[[i]])
    errors(value[[i]]) <- e[[i]]
  }
  value
}

#' \code{type_sum} for Tidy \code{tibble} Printing
#'
#' S3 method for \code{quantities} objects.
#'
#' @param x object of class quantities
#' @param ... ignored
#'
#' @export type_sum.quantities
type_sum.quantities <- function(x, ...) "quantities"

#' Coerce to a Matrix
#'
#' S3 method for \code{quantities} objects (see \code{\link{as.matrix}}).
#'
#' @inheritParams base::matrix
#'
#' @examples
#' as.matrix(set_quantities(1:3, m/s, 0.1))
#'
#' @export
as.matrix.quantities <- function(x, ...) {
  value <- reclass(NextMethod())
  attr(value, "units") <- units(x)
  value
}

#' Matrix Transpose
#'
#' S3 method for \code{quantities} objects (see \code{\link{t}}).
#'
#' @inheritParams base::t
#'
#' @examples
#' a <- matrix(1:30, 5, 6)
#' quantities(a) <- list("m/s", 1:30)
#' t(a)
#'
#' @export
t.quantities <- function(x) reclass(NextMethod())

#' Combine R Objects by Rows or Columns
#'
#' S3 methods for \code{quantities} objects (see \code{\link[base]{cbind}}).
#'
#' @inheritParams base::cbind
#' @name cbind.quantities
#'
#' @seealso \code{\link{c.quantities}}
#'
#' @examples
#' x <- set_quantities(1, m/s, 0.1)
#' y <- set_quantities(1:3, m/s, 0.2)
#' z <- set_quantities(8:10, m/s, 0.1)
#' (m <- cbind(x, y)) # the '1' (= shorter vector) is recycled
#' (m <- cbind(m, z)[, c(1, 3, 2)]) # insert a column
#' (m <- rbind(m, z)) # insert a row
#'
#' @export
cbind.quantities <- function(..., deparse.level = 1) {
  dots <- list(...)
  u <- units(dots[[1]])
  .convert_to_first_arg(dots)

  nm <- names(as.list(match.call()))
  nm <- nm[nm != "" & nm != "deparse.level"]
  if (is.null(nm))
    names(dots) <- sapply(substitute(list(...))[-1], deparse)
  else names(dots) <- nm

  call <- as.character(match.call()[[1]])
  assign(call, utils::getS3method(call, "errors"))
  value <- do.call(call, c(dots, deparse.level=deparse.level))
  attr(value, "units") <- u
  reclass(value)
}

#' @rdname cbind.quantities
#' @export
rbind.quantities <- cbind.quantities
