# Handle Correlations Between `quantities` Objects

Methods to set or retrieve correlations or covariances between
`quantities` objects.

## Usage

``` r
# S3 method for class 'quantities'
correl(x, y)

# S3 method for class 'quantities'
correl(x, y) <- value

# S3 method for class 'quantities'
covar(x, y)

# S3 method for class 'quantities'
covar(x, y) <- value
```

## Arguments

- x:

  an object of class `quantities`.

- y:

  an object of class `quantities` of the same length as `x`.

- value:

  a compatible object of class `units` of length 1 or the same length as
  `x`. For correlations, this means a unitless vector (a numeric vector
  is also accepted in this case). For covariances, this means the same
  magnitude as `x*y`.

## See also

[`correl`](https://r-quantities.github.io/errors/reference/correl.html).

## Examples

``` r
x <- set_quantities(1:10, m/s, 0.1)
y <- set_quantities(10:1, km/h, 0.2)
correl(x, y) <- 0.1 # accepted
correl(x, y) <- set_units(0.1) # recommended
correl(x, y)
#> Units: [1]
#>  [1] 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1
covar(x, y)
#> Units: [km*m/(h*s)]
#>  [1] 0.002 0.002 0.002 0.002 0.002 0.002 0.002 0.002 0.002 0.002
```
