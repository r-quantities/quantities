#' @rdname groupGeneric.quantities
#'
#' @examples
#' a <- set_quantities(1:3, m/s, 0.1)
#' b <- set_quantities(1:3, m/s, 0.1)
#' a + b
#' a * b
#' a / b
#' a = set_quantities(1:5, m, 0.1)
#' a %/% a
#' a %/% set_quantities(2)
#' set_quantities(1:5, m^2, 0.1) %/% set_quantities(2, m, 0.1)
#' a %% a
#' a %% set_quantities(2)
#' @export
Ops.quantities <- function(e1, e2) reclass(NextMethod())
