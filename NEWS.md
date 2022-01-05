# quantities 0.2.0

- Add compatibility between `errors::geom_errors` (r-quantities/errors#52) and
  `units::scale_[x|y]_units` (r-quantities/units#294) (#13).
- Fix warning in `as.list.quantities`.
- Add plotting examples to introductory vignette.
- Add support for errors with units (#14). For objects of class `quantities` or
  `units`, the `errors()` method now returns a `units` object that matches the
  units of `x`. Methods `errors<-()` and `set_errors()` assume that the provided
  uncertainty (`value`) has the same units as `x` (for backwards compatibility).
  However, it is a best practice to provide a `value` with explicit units. In
  this way, uncertainty can be provided in different (but compatible) units,
  and it will be automatically converted to the units of `x` (see
  `help("errors", "quantities")`).
- Add support for correlations and covariances with units for objects of class
  `quantities` (as part of #14).
- Remove some superfluous `.quantities` methods (as part of #14).
- Implement methods for `duplicated`, `anyDuplicated` and `unique` (#12).

# quantities 0.1.6

- Fix compatibility with `units` 0.7-0.

# quantities 0.1.5

- Fix compatibility with `dplyr` 1.0.0 (#8), coordinated with units and errors.
- Fix uncertainty propagation for mixed scaling+offset unit conversion, such
  as Celsius to Fahrenheit (#9).

# quantities 0.1.4

- Add compatibility with upcoming tibble v3.0.0 (r-quantities/units#225).
- Implement prettier `str` print (#5).
- Implement drop method for data frames.
- Fix introduction vignette for `dplyr` >= 1.0.0.

# quantities 0.1.3

- Add CITATION to `units` and `errors` in the *R Journal*.
- Fix unit simplification in arithmetical operations (#3).

# quantities 0.1.2

- Add delayed S3 registration mechanism for R >= 3.6.0 (a9bb97e).

# quantities 0.1.1

- Initial CRAN release.
