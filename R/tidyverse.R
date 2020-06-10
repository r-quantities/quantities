
vec_proxy.quantities <- function(x, ...) {
  vec_proxy.errors <- utils::getS3method("vec_proxy", "errors", envir = asNamespace("vctrs"))
  vec_proxy.errors(drop_units(x))
}

vec_restore.quantities <- function(x, to, ...) {
  vec_restore.errors <- utils::getS3method("vec_restore", "errors", envir = asNamespace("vctrs"))

  out <- vec_restore.errors(x, drop_units(to))
  set_quantities(out, units(to), errors(out), mode = "standard")
}

vec_ptype2.quantities.quantities <- function(x, y, ...) {
  x_units <- drop_errors(x)
  y_units <- drop_errors(y)
  common <- vctrs::vec_ptype2(x_units, y_units, ...)

  set_quantities(common, units(common), mode = "standard")
}

vec_cast.quantities.quantities <- function(x, to, ...) {
  to_units <- units(to)

  # First set units and errors. Must happen first in case this causes
  # `x` to become fractional (which should cause an error if `to` is
  # integer).
  out <- set_units(x, to_units, mode = "standard")
  out_errors <- errors(out)

  # Now cast base type
  out_bare <- drop_quantities(out)
  to_bare <- drop_quantities(to)
  out <- vctrs::vec_cast(out_bare, to_bare, ...)

  # Set quantities back
  set_quantities(out, to_units, out_errors, mode = "standard")
}


#nocov start
register_all_s3_methods <- function() {
  register_s3_method("vctrs::vec_proxy", "quantities")
  register_s3_method("vctrs::vec_restore", "quantities")
  register_s3_method("vctrs::vec_ptype2", "quantities.quantities")
  register_s3_method("vctrs::vec_cast", "quantities.quantities")
}

register_s3_method <- function(generic, class, fun=NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]

  if (is.null(fun))
    fun <- get(paste0(generic, ".", class), envir=parent.frame())
  stopifnot(is.function(fun))

  if (package %in% loadedNamespaces())
    registerS3method(generic, class, fun, envir=asNamespace(package))

  # Always register hook in case package is later unloaded & reloaded
  setHook(packageEvent(package, "onLoad"), function(...)
    registerS3method(generic, class, fun, envir=asNamespace(package)))
}
# nocov end
