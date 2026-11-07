# Handle Measurement Units and Uncertainty on a Numeric Vector

Set or retrieve measurement units and uncertainty to/from numeric
vectors.

## Usage

``` r
quantities(x)

quantities(x) <- value

set_quantities(x, unit, errors = 0, ...,
  mode = units_options("set_units_mode"))
```

## Arguments

- x:

  a numeric object, or object of class `quantities`, `units` or
  `errors`.

- value:

  a list of two components: an object of class `units` or
  `symbolic_units` (see
  [`units`](https://r-quantities.github.io/units/reference/units.html)),
  and a numeric vector of length 1 or the same length as `x` (see
  [`errors`](https://r-quantities.github.io/errors/reference/errors.html)).

- unit:

  a `units` object, or something coercible to one with `as_units` (see
  [`set_units`](https://r-quantities.github.io/units/reference/units.html)).

- errors:

  a numeric vector of length 1 or the same length as `x` (see
  [`set_errors`](https://r-quantities.github.io/errors/reference/errors.html)).

- ...:

  passed on to other methods.

- mode:

  if `"symbols"` (the default), then unit is constructed from the
  expression supplied. Otherwise, if `mode = "standard"`, standard
  evaluation is used for the supplied value. This argument can be set
  via a global option `units_options(set_units_mode = "standard")`.

## Details

`quantities` returns a named list with the `units` and `errors`
attributes.

`` `quantities<-` `` sets the units and error values (and converts `x`
into an object of class `quantities`). `set_quantities` is a
pipe-friendly version of `` `quantities<-` `` and returns an object of
class `quantities`.

## See also

[`errors`](errors.md), [`units`](units.md),
[`groupGeneric.quantities`](groupGeneric.quantities.md).
[`Extract.quantities`](Extract.quantities.md),
[`c.quantities`](c.quantities.md),
[`rep.quantities`](rep.quantities.md),
[`cbind.quantities`](cbind.quantities.md).
[`as.data.frame.quantities`](as.data.frame.quantities.md),
[`as.matrix.quantities`](as.matrix.quantities.md),
[`t.quantities`](t.quantities.md).

## Examples

``` r
x = 1:3
class(x)
#> [1] "integer"
x
#> [1] 1 2 3
quantities(x) <- list("m/s", 0.1)
class(x)
#> [1] "quantities" "units"      "errors"    
x
#> Units: [m/s]
#> Errors: 0.1 0.1 0.1
#> [1] 1 2 3

(x <- set_quantities(x, m/s, seq(0.1, 0.3, 0.1)))
#> Units: [m/s]
#> Errors: 0.1 0.2 0.3
#> [1] 1 2 3
```
