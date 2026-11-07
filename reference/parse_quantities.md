# Parse Units and Errors

Functions to parse character vectors into quantities.

## Usage

``` r
parse_quantities(x, decimal_mark)

parse_units(x, decimal_mark)

parse_errors(x, decimal_mark)
```

## Arguments

- x:

  a character vector to parse.

- decimal_mark:

  the dot (`.`) if not provided.

## Value

A `quantities`, `units` or `errors` object respectively.

## Details

Each `parse_*()` function returns an object of the corresponding type,
no matter what it is found. This means that, for `parse_units`, if
errors are found, they are dropped with a warning. Similarly for
`parse_errors`, if units are found, they are dropped with a warning. On
the other hand, `parse_quantities` always returns a valid `quantities`
object, even if no errors or units are found (then, zero error and
dimensionless units are applied).

## Examples

``` r
parse_quantities("(1.6021766208 +/- .0000000098) e-19 C")
#> 1.60217662(1)e-19 [C]
parse_quantities("1.6021766208(98) e-19 C")
#> 1.60217662(1)e-19 [C]
parse_units("1.6021766208 e-19 C")
#> 1.602177e-19 [C]
parse_errors("1.6021766208(98) e-19")
#> 1.60217662(1)e-19

# quantities are converted to the first unit
parse_quantities(c("12.34(2) m/s", "36.5(1) km/h"))
#> Units: [m/s]
#> Errors: 0.02000000 0.02777778
#> [1] 12.34000 10.13889

# or kept as a list of mixed units
parse_quantities(c("1.02(5) g", "2.51(0.01) V", "(3.23 +/- 0.12) m"))
#> Mixed units: g (1), V (1), m (1) 
#> 1.02(5) [g], 2.51(1) [V], 3.2(1) [m] 
```
