# <img src="https://avatars1.githubusercontent.com/u/32303769?s=40&v=4"> Quantity Calculus for R

<!-- badges: start -->
[![Build Status](https://github.com/r-quantities/quantities/workflows/build/badge.svg)](https://github.com/r-quantities/quantities/actions)
[![Coverage Status](https://codecov.io/gh/r-quantities/quantities/branch/master/graph/badge.svg)](https://app.codecov.io/gh/r-quantities/quantities) 
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/quantities)](https://cran.r-project.org/package=quantities)
[![Downloads](https://cranlogs.r-pkg.org/badges/quantities)](https://cran.r-project.org/package=quantities)
<!-- badges: end -->

The **quantities** package provides integration of the 'units' and 'errors' packages for a complete quantity calculus system for R vectors, matrices and arrays, with automatic propagation, conversion, derivation and simplification of magnitudes and uncertainties.

## Documentation

Blog posts:

1. [Quantities for R -- First working prototype](https://r-spatial.org/r/2018/03/01/quantities-first-prototype.html), Mar 1, 2018.
2. [Using quantities to parse data with units and errors](https://r-spatial.org/r/2018/05/07/parsing-quantities.html), May 7, 2018.
3. [Data wrangling operations with quantities](https://r-spatial.org/r/2018/06/27/wrangling-quantities.html), Jun 27, 2018.
4. [Quantities for R -- Ready for a CRAN release](https://r-spatial.org/r/2018/08/31/quantities-final.html), Aug 31, 2018.

Vignettes:

- [A Guide to Working with Quantities](https://r-quantities.github.io/quantities/articles/introduction.html).
- [Parsing Quantities](https://r-quantities.github.io/quantities/articles/parsing.html).

Papers:

- Edzer Pebesma, Thomas Mailund and James Hiebert (2016). "Measurement Units in R." _The R Journal_, 8 (2), 486--494. DOI: [10.32614/RJ-2016-061](https://doi.org/10.32614/RJ-2016-061)
- Iñaki Ucar, Edzer Pebesma and Arturo Azcorra (2018). "Measurement Errors in R." _The R Journal_, 10 (2), 549--557. DOI: [10.32614/RJ-2018-075](https://doi.org/10.32614/RJ-2018-075)

## Installation

Install the release version from CRAN:

```r
install.packages("quantities")
```

The installation from GitHub requires the [remotes](https://cran.r-project.org/package=remotes) package.

```r
# install.packages("remotes")
remotes::install_github(paste("r-quantities", c("units", "errors", "quantities"), sep="/"))
```

## Acknowledgement

This project gratefully acknowledges financial [support](https://www.r-consortium.org/projects) from the

<a href="https://www.r-consortium.org/all-projects/awarded-projects">
<img src="http://pebesma.staff.ifgi.de/RConsortium_Horizontal_Pantone.png" width="300">
</a>
