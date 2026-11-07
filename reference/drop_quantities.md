# Drop Units and Errors

Drop Units and Errors

## Usage

``` r
drop_quantities(x)

# S3 method for class 'quantities'
drop_units(x)

# S3 method for class 'quantities'
drop_errors(x)

# S3 method for class 'data.frame'
drop_quantities(x)
```

## Arguments

- x:

  a `quantities` object.

## Value

the numeric without any `units` or `errors` attributes, while preserving
other attributes like dimensions or other classes.

## Details

`drop_quantities` is equivalent to `quantities(x) <- NULL` or
`set_quantities(x, NULL, NULL)`. `drop_units` is equivalent to
`units(x) <- NULL` or `set_units(x, NULL)`. `drop_errors` is equivalent
to `errors(x) <- NULL` or `set_errors(x, NULL)`.
