# Coerce to a Data Frame

S3 method for `quantities` objects (see
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html)).

## Usage

``` r
# S3 method for class 'quantities'
as.data.frame(x, ...)
```

## Arguments

- x:

  any R object.

- ...:

  additional arguments to be passed to or from methods.

## Examples

``` r
x <- set_quantities(1:3, m/s, 0.1)
y <- set_quantities(4:6, m/s, 0.2)
(z <- cbind(x, y))
#> Units: [m/s]
#> Errors: 0.1 0.1 0.1 0.2 0.2 ...
#>      x y
#> [1,] 1 4
#> [2,] 2 5
#> [3,] 3 6
as.data.frame(z)
#>              x            y
#> 1 1.0(1) [m/s] 4.0(2) [m/s]
#> 2 2.0(1) [m/s] 5.0(2) [m/s]
#> 3 3.0(1) [m/s] 6.0(2) [m/s]
```
