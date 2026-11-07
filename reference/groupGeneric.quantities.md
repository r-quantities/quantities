# S3 Group Generic Functions

`Math`, `Ops` and `Summary` group generic methods for `quantities`
objects (see [`groupGeneric`](https://rdrr.io/r/base/groupGeneric.html)
for a comprehensive list of available methods).

## Usage

``` r
# S3 method for class 'quantities'
Math(x, ...)

# S3 method for class 'quantities'
Ops(e1, e2)

# S3 method for class 'quantities'
Summary(..., na.rm = FALSE)
```

## Arguments

- x, e1, e2:

  objects.

- ...:

  further arguments passed to methods.

- na.rm:

  logical: should missing values be removed?

## Details

See
[`groupGeneric.errors`](https://r-quantities.github.io/errors/reference/groupGeneric.errors.html),
[`Ops.units`](https://r-quantities.github.io/units/reference/Ops.units.html),
[`Math.units`](https://r-quantities.github.io/units/reference/Math.units.html),
for further details.

## Examples

``` r
x <- set_quantities(1:3, m/s, 0.1)
log(x)
#> Units: [ln(re 1 m.s-1)]
#> Errors: 0.10000000 0.05000000 0.03333333
#> [1] 0.0000000 0.6931472 1.0986123
cumsum(x)
#> Units: [m/s]
#> Errors: 0.1000000 0.1414214 0.1732051
#> [1] 1 3 6
cumprod(x)
#> Warning: Operation cumprod not meaningful for units
#> Errors: 0.1000000 0.2236068 0.7000000
#> [1] 1 2 6

a <- set_quantities(1:3, m/s, 0.1)
b <- set_quantities(1:3, m/s, 0.1)
a + b
#> Units: [m/s]
#> Errors: 0.1414214 0.1414214 0.1414214
#> [1] 2 4 6
a * b
#> Units: [m^2/s^2]
#> Errors: 0.1414214 0.2828427 0.4242641
#> [1] 1 4 9
a / b
#> Units: [1]
#> Errors: 0.14142136 0.07071068 0.04714045
#> [1] 1 1 1
a = set_quantities(1:5, m, 0.1)
a %/% a
#> Units: [1]
#> Errors: 0.000000e+00 0.000000e+00 0.000000e+00 0.000000e+00 4.656613e-10
#> [1] 1 1 1 1 1
a %/% set_quantities(2)
#> Units: [m]
#> Errors: 0.55 0.05 0.55 0.05 0.55
#> [1] 0 1 1 2 2
set_quantities(1:5, m^2, 0.1) %/% set_quantities(2, m, 0.1)
#> Units: [m]
#> Errors: 0.55590170 0.07071068 0.59013878 0.11180340 0.63462912
#> [1] 0 1 1 2 2
a %% a
#> Units: [m]
#> Errors: 0.1414214 0.1414214 0.1414214 0.1414214 0.1414214
#> [1] 0 0 0 0 0
a %% set_quantities(2)
#> Units: [m]
#> Errors: 1.1045361 0.1414214 1.1045361 0.1414214 1.1045361
#> [1] 1 0 1 0 1
c(min(x), max(x))
#> Units: [m/s]
#> [1] 0.9 3.1
range(x)
#> Units: [m/s]
#> [1] 0.9 3.1
sum(x)
#> 6.0(2) [m/s]
```
