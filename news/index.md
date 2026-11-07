# Changelog

## quantities 0.2.4

- Improve `cbind` arg name deparsing.
- Fix [`str()`](https://rdrr.io/r/utils/str.html) method, which now
  strips the ‘errors’ class tag whether or not the default method adds a
  leading space, so it works with R-devel r90805

## quantities 0.2.3

CRAN release: 2025-01-18

- Fixes for `covar` and `correl` implementations.

## quantities 0.2.2

CRAN release: 2024-07-29

- Performance improvements for `data.frame` methods.
- Update `ggplot2` tests (r-quantities/units#367).

## quantities 0.2.1

CRAN release: 2023-04-26

- Fix a couple of tests (r-quantities/units#324).

## quantities 0.2.0

CRAN release: 2022-12-05

- Add compatibility between
  [`errors::geom_errors`](https://r-quantities.github.io/errors/reference/geom_errors.html)
  (r-quantities/errors#52) and `units::scale_[x|y]_units`
  (r-quantities/units#294)
  ([\#13](https://github.com/r-quantities/quantities/issues/13)).
- Fix warning in `as.list.quantities`.
- Add plotting examples to introductory vignette.
- Add support for errors with units
  ([\#14](https://github.com/r-quantities/quantities/issues/14)). For
  objects of class `quantities` or `units`, the
  [`errors()`](../reference/errors.md) method now returns a `units`
  object that matches the units of `x`. Methods `errors<-()` and
  [`set_errors()`](https://r-quantities.github.io/errors/reference/errors.html)
  assume that the provided uncertainty (`value`) has the same units as
  `x` (for backwards compatibility). However, it is a best practice to
  provide a `value` with explicit units. In this way, uncertainty can be
  provided in different (but compatible) units, and it will be
  automatically converted to the units of `x` (see
  [`help("errors", "quantities")`](../reference/errors.md)).
- Add support for correlations and covariances with units for objects of
  class `quantities` (as part of
  [\#14](https://github.com/r-quantities/quantities/issues/14)).
- Remove some superfluous `.quantities` methods (as part of
  [\#14](https://github.com/r-quantities/quantities/issues/14)).
- Implement methods for `duplicated`, `anyDuplicated` and `unique`
  ([\#12](https://github.com/r-quantities/quantities/issues/12)).

## quantities 0.1.6

CRAN release: 2021-02-21

- Fix compatibility with `units` 0.7-0.

## quantities 0.1.5

CRAN release: 2020-06-14

- Fix compatibility with `dplyr` 1.0.0
  ([\#8](https://github.com/r-quantities/quantities/issues/8)),
  coordinated with units and errors.
- Fix uncertainty propagation for mixed scaling+offset unit conversion,
  such as Celsius to Fahrenheit
  ([\#9](https://github.com/r-quantities/quantities/issues/9)).

## quantities 0.1.4

CRAN release: 2020-06-06

- Add compatibility with upcoming tibble v3.0.0
  (r-quantities/units#225).
- Implement prettier `str` print
  ([\#5](https://github.com/r-quantities/quantities/issues/5)).
- Implement drop method for data frames.
- Fix introduction vignette for `dplyr` \>= 1.0.0.

## quantities 0.1.3

CRAN release: 2019-10-31

- Add CITATION to `units` and `errors` in the *R Journal*.
- Fix unit simplification in arithmetical operations
  ([\#3](https://github.com/r-quantities/quantities/issues/3)).

## quantities 0.1.2

CRAN release: 2018-12-05

- Add delayed S3 registration mechanism for R \>= 3.6.0 (a9bb97e).

## quantities 0.1.1

CRAN release: 2018-09-30

- Initial CRAN release.
