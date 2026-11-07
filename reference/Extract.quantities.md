# Extract or Replace Parts of an Object

S3 operators to extract or replace parts of `quantities` objects.

## Usage

``` r
# S3 method for class 'quantities'
x[...]

# S3 method for class 'quantities'
x[[...]]

# S3 method for class 'quantities'
x[...] <- value

# S3 method for class 'quantities'
x[[...]] <- value
```

## Arguments

- x:

  object from which to extract element(s) or in which to replace
  element(s).

- ...:

  additional arguments to be passed to base methods (see
  [`Extract`](https://rdrr.io/r/base/Extract.html)).

- value:

  typically an array-like R object of a similar class as `x`.

## Examples

``` r
x <- set_quantities(1:3, m/s, 0.1)
y <- set_quantities(4:6, m/s, 0.2)
(z <- rbind(x, y))
#> Units: [m/s]
#> Errors: 0.1 0.2 0.1 0.2 0.1 ...
#>   [,1] [,2] [,3]
#> x    1    2    3
#> y    4    5    6
z[2, 2]
#> 5.0(2) [m/s]
z[2, 2] <- -1
errors(z[[1, 2]]) <- 0.8 # assumes same unit
errors(z[[2, 2]]) <- set_units(80, cm/s)
z[, 2]
#> Units: [m/s]
#> Errors: 0.8 0.8
#>  x  y 
#>  2 -1 
```
