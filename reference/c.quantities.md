# Combine Values into a Vector or List

S3 method for `quantities` objects (see
[`c`](https://rdrr.io/r/base/c.html)).

## Usage

``` r
# S3 method for class 'quantities'
c(...)
```

## Arguments

- ...:

  objects to be concatenated. All
  [`NULL`](https://rdrr.io/r/base/NULL.html) entries are dropped before
  method dispatch unless at the very beginning of the argument list.

## Examples

``` r
c(set_quantities(1, m/s, 0.2), set_quantities(30, km/h, 0.1))
#> Units: [m/s]
#> Errors: 0.20000000 0.02777778
#> [1] 1.000000 8.333333
```
