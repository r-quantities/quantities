
vec_restore.quantities <- function(x, to, ...) {
  # Delegate errors restoration
  x <- NextMethod()
  set_quantities(x, units(to), errors(x), mode = "standard")
}

register_tidyverse_methods = function() {
  s3_register("vctrs::vec_restore", "quantities")
}
