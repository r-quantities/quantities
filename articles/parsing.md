# Parsing Quantities

## Introduction

The [BIPM](https://www.bipm.org/) (*Bureau International des Poids et
Mesures*) is the international *authority* on measurement units and
uncertainty. The Joint Committee for Guides in Metrology (JCGM),
dependent on the BIPM together with other international standardisation
bodies, maintains two fundamental guides in metrology: the
[VIM](https://www.bipm.org/en/committees/jc/jcgm/publications) (“The
International Vocabulary of Metrology – Basic and General Concepts and
Associated Terms”) and the
[GUM](https://www.bipm.org/en/committees/jc/jcgm/publications)
(“Evaluation of Measurement Data – Guide to the Expression of
Uncertainty in Measurement”). The latter defines four ways of reporting
standard uncertainty. For example, if we are reporting a nominal mass
$`m_S`$ of 100 g with some uncertainty $`u_c`$:

1.  $`m_S`$ = 100.02147 g, $`u_c`$ = 0.35 mg; that is, quantity an
    uncertainty are reported separatedly, and thus they may be expressed
    in different units.
2.  $`m_S`$ = 100.02147(35) g, where the number in parentheses is the
    value of $`u_c`$ referred to the corresponding last digits of the
    reported quantity.
3.  $`m_S`$ = 100.02147(0.00035) g, where the number in parentheses is
    the value of $`u_c`$ expressed in the unit of the reported quantity.
4.  $`m_S`$ = (100.02147 $`\pm`$ 0.00035), where the number following
    the symbol $`\pm`$ is the value of $`u_c`$ in the unit of the
    reported quantity.

The second scheme is the most compact one, and it is the default
reporting mode in the `errors` package. The fourth scheme is also
supported given that it is a very extended notation, but the GUM
discourages its use to prevent confusion with confidence intervals.

In the same lines, the BIMP also publishes the [International System of
Units](https://www.bipm.org/en/measurement-units/) (SI), which consist
of seven base units and derived units, many of them with special names
and symbols. Units are reported after the corresponding quantity using
products of powers of symbols (e.g., 1 N = 1 m kg s-2).

## Available parsers

The `quantities` package provides three methods that parse units and
uncertainty following the GUM’s recommendations:

- [`parse_quantities()`](../reference/parse_quantities.md): The returned
  value is always a `quantities` object.
  - If no uncertainty was found, a zero error is assumed for all values.
  - If no units were found, all values are supposed to be unitless.
- [`parse_errors()`](../reference/parse_quantities.md): The returned
  value is always an `errors` object.
  - If no uncertainty was found, a zero error is assumed for all values.
  - If units were found, a warning is emitted.
- [`parse_units()`](../reference/parse_quantities.md): The returned
  value is always a `units` object.
  - If uncertaint was found, a warning is emitted.
  - If no units were found, all values are supposed to be unitless.

Given a rectangular data file, such as a CSV file, it can be read with
any CSV reader (e.g., base `read.csv`, `readr`’s `read_csv` or
`data.table`’s `fread`). Then, a proper parser can be used to convert
columns as required.

``` r

(d.quantities <- d.units <- d.errors <- read.csv(textConnection("
quantities,        units,  errors
1.02(5) g,         1.02 g, 1.02(5)
2.51(0.01) V,      2.51 V, 2.51(0.01)
(3.23 +/- 0.12) m, 3.23 m, 3.23 +/- 0.12"), stringsAsFactors=FALSE))
#>          quantities           units         errors
#> 1         1.02(5) g          1.02 g        1.02(5)
#> 2      2.51(0.01) V          2.51 V     2.51(0.01)
#> 3 (3.23 +/- 0.12) m          3.23 m  3.23 +/- 0.12
```

``` r

library(quantities)
#> Loading required package: units
#> udunits database from /usr/share/xml/udunits/udunits2.xml
#> Loading required package: errors

for (name in names(d.quantities)) {
  message(name)
  d.quantities[[name]] <- parse_quantities(d.quantities[[name]])
  d.units[[name]] <- parse_units(d.units[[name]])
  d.errors[[name]] <- parse_errors(d.errors[[name]])
}
#> quantities
#> Warning in parse_units(d.units[[name]]): errors present but ignored
#> Warning in parse_errors(d.errors[[name]]): units present but ignored
#> units
#> Warning in parse_errors(d.errors[[name]]): units present but ignored
#> errors
#> Warning in parse_units(d.units[[name]]): errors present but ignored

d.quantities
#>    quantities       units      errors
#> 1 1.02(5) [g] 1.02(0) [g] 1.02(5) [1]
#> 2 2.51(1) [V] 2.51(0) [V] 2.51(1) [1]
#> 3  3.2(1) [m] 3.23(0) [m]  3.2(1) [1]
d.units
#>   quantities    units   errors
#> 1   1.02 [g] 1.02 [g] 1.02 [1]
#> 2   2.51 [V] 2.51 [V] 2.51 [1]
#> 3   3.23 [m] 3.23 [m] 3.23 [1]
d.errors
#>   quantities   units  errors
#> 1    1.02(5) 1.02(0) 1.02(5)
#> 2    2.51(1) 2.51(0) 2.51(1)
#> 3     3.2(1) 3.23(0)  3.2(1)
```
