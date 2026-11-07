# Handle Measurement Uncertainty on a Numeric Vector

Set or retrieve measurement uncertainty to/from numeric vectors
(extensions to the errors package for `quantities` and `units` objects).

## Usage

``` r
# S3 method for class 'units'
errors(x)

# S3 method for class 'mixed_units'
errors(x)

# S3 method for class 'units'
errors(x) <- value

# S3 method for class 'mixed_units'
errors(x) <- value

# S3 method for class 'units'
set_errors(x, value = 0)

# S3 method for class 'mixed_units'
set_errors(x, value = 0)

# S3 method for class 'units'
errors_max(x)

# S3 method for class 'units'
errors_min(x)
```

## Arguments

- x:

  a numeric object, or object of class `quantities`, `units` or
  `errors`.

- value:

  a numeric vector or `units` object of length 1, or the same length as
  `x` (see details).

## Details

For objects of class `quantities` or `units`, the `errors()` method
returns a `units` object that matches the units of `x`. Methods
`` `errors<-`() `` and `set_errors()` assume that the provided
uncertainty (`value`) has the same units as `x`. However, it is a best
practice to provide a `value` with explicit units. In this way,
uncertainty can be provided in different (but compatible) units, and it
will be automatically converted to the units of `x` (see examples
below).

## See also

[`errors`](https://r-quantities.github.io/errors/reference/errors.html).

## Examples

``` r
x <- set_units(1:5, m)
errors(x) <- 0.01 # implicit units, same as x
errors(x)
#> Units: [m]
#> [1] 0.01 0.01 0.01 0.01 0.01
errors(x) <- set_units(1, cm) # explicit units
errors(x)
#> Units: [m]
#> [1] 0.01 0.01 0.01 0.01 0.01
```
