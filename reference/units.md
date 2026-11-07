# Handle Measurement Units on a Numeric Vector

Set or retrieve measurement units to/from numeric vectors and convert
units (extensions to the units package for `quantities` and `errors`
objects).

## Usage

``` r
# S3 method for class 'quantities'
units(x) <- value

# S3 method for class 'errors'
units(x) <- value

# S3 method for class 'errors'
set_units(x, value, ...,
  mode = units_options("set_units_mode"))

# S3 method for class 'quantities'
mixed_units(x, values, ...)

# S3 method for class 'errors'
mixed_units(x, values, ...)
```

## Arguments

- x:

  a numeric object, or object of class `quantities`, `units` or
  `errors`.

- value:

  object of class `units` or `symbolic_units`, or in the case of
  `set_units` expression with symbols (see examples).

- ...:

  passed on to other methods.

- mode:

  if `"symbols"` (the default), then unit is constructed from the
  expression supplied. Otherwise, if `mode = "standard"`, standard
  evaluation is used for the supplied value. This argument can be set
  via a global option `units_options(set_units_mode = "standard")`.

- values:

  character vector with units encodings, or list with symbolic units of
  class `mixed_symbolic_units`

## Details

For objects of class `quantities`, methods `` `units<-`() `` and
`set_units()` automatically convert the associated uncertainty to the
new unit (see examples below).

## See also

[`units`](https://r-quantities.github.io/units/reference/units.html),
[`set_units`](https://r-quantities.github.io/units/reference/units.html).

## Examples

``` r
(x <- set_quantities(1:5, m, 0.01))
#> Units: [m]
#> Errors: 0.01 0.01 0.01 0.01 0.01
#> [1] 1 2 3 4 5
set_units(x, cm)
#> Units: [cm]
#> Errors: 1 1 1 1 1
#> [1] 100 200 300 400 500
```
