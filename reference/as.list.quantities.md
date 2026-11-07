# Coerce to a List

S3 method for `quantities` objects (see
[`as.list`](https://rdrr.io/r/base/list.html)).

## Usage

``` r
# S3 method for class 'quantities'
as.list(x, ...)
```

## Arguments

- x:

  object to be coerced or tested.

- ...:

  objects, possibly named.

## Examples

``` r
x <- set_quantities(1:3, m/s, 0.1)
as.list(x)
#> [[1]]
#> 1.0(1) [m/s]
#> 
#> [[2]]
#> 2.0(1) [m/s]
#> 
#> [[3]]
#> 3.0(1) [m/s]
#> 
```
