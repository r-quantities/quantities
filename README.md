# <img src="https://avatars1.githubusercontent.com/u/32303769?s=40&v=4"> quantities: Quantity Calculus for R Vectors

[![Build Status](https://travis-ci.org/r-quantities/quantities.svg?branch=master)](https://travis-ci.org/r-quantities/quantities) [![Coverage Status](https://codecov.io/gh/r-quantities/quantities/branch/master/graph/badge.svg)](https://codecov.io/gh/r-quantities/quantities) [![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/quantities)](https://cran.r-project.org/package=quantities) [![Downloads](https://cranlogs.r-pkg.org/badges/quantities)](https://cran.r-project.org/package=quantities)

The **quantities** package provides integration of the 'units' and 'errors' packages for a complete quantity calculus system for R vectors, matrices and arrays, with automatic propagation, conversion, derivation and simplification of magnitudes and uncertainties.

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
