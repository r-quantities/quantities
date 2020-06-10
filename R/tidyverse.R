
vec_restore.quantities <- function(x, to, ...) {
  # Delegate errors restoration
  x <- NextMethod()
  set_quantities(x, units(to), errors(x), mode = "standard")
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

register_tidyverse_methods = function() {
  s3_register("vctrs::vec_restore", "quantities")
  s3_register("vctrs::vec_ptype2", "quantities.quantities")
  s3_register("vctrs::vec_cast", "quantities.quantities")
}
