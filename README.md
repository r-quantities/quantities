# <img src="https://avatars1.githubusercontent.com/u/32303769?s=40&v=4"> quantities: Quantity Calculus for R Vectors

[![Build Status](https://travis-ci.org/r-quantities/quantities.svg?branch=master)](https://travis-ci.org/r-quantities/quantities) [![Coverage Status](https://codecov.io/gh/r-quantities/quantities/branch/master/graph/badge.svg)](https://codecov.io/gh/r-quantities/quantities) [![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/quantities)](https://cran.r-project.org/package=quantities) [![Downloads](https://cranlogs.r-pkg.org/badges/quantities)](https://cran.r-project.org/package=quantities)

The **quantities** package provides integration of the 'units' and 'errors' packages for a complete quantity calculus system for R vectors, matrices and arrays, with automatic propagation, conversion, derivation and simplification of magnitudes and uncertainties.

## Documentation

Blog posts:

1. [Quantities for R -- First working prototype](https://www.r-spatial.org/r/2018/03/01/quantities-first-prototype.html), Mar 1, 2018.
2. [Using quantities to parse data with units and errors](https://www.r-spatial.org/r/2018/05/07/parsing-quantities.html), May 7, 2018.
3. [Data wrangling operations with quantities](https://www.r-spatial.org/r/2018/06/27/wrangling-quantities.html), Jun 27, 2018.
4. [Quantities for R -- Ready for a CRAN release](https://www.r-spatial.org/r/2018/08/31/quantities-final.html), Aug 31, 2018.

Vignettes:

- [A Guide to Working with Quantities](https://github.com/r-quantities/quantities/blob/master/vignettes/introduction.Rmd).
- [Parsing Quantities](https://github.com/r-quantities/quantities/blob/master/vignettes/parsing.Rmd).

Papers:

- Edzer Pebesma, Thomas Mailund and James Hiebert (2016). "Measurement Units in R." _The R Journal_, 8 (2), 486--494 [[online](https://journal.r-project.org/archive/2016/RJ-2016-061/index.html)].
- Iñaki Ucar, Edzer Pebesma, Arturo Azcorra (2018). "Measurement Errors in R." arXiv [[online](https://arxiv.org/abs/1804.08552)].

## Installation

The installation from GitHub requires the [remotes](https://cran.r-project.org/package=remotes) package.

```r
# install.packages("remotes")
remotes::install_github(paste("r-quantities", c("units", "errors", "quantities"), sep="/"))
```

## Acknowledgement

This project gratefully acknowledges financial [support](https://www.r-consortium.org/projects) from the

<a href="https://www.r-consortium.org/projects/awarded-projects">
<img src="http://pebesma.staff.ifgi.de/RConsortium_Horizontal_Pantone.png" width="300">
</a>
