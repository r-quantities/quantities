# Coerce to a Matrix

S3 method for `quantities` objects (see
[`as.matrix`](https://rdrr.io/r/base/matrix.html)).

## Usage

``` r
# S3 method for class 'quantities'
as.matrix(x, ...)
```

## Arguments

- x:

  an R object.

- ...:

  additional arguments to be passed to or from methods.

## Examples

``` r
as.matrix(set_quantities(1:3, m/s, 0.1))
#> Units: [m/s]
#> Errors: 0.1 0.1 0.1
#>      [,1]
#> [1,]    1
#> [2,]    2
#> [3,]    3
```
