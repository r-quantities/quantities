% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/units.R
\name{units}
\alias{units}
\alias{units<-.quantities}
\alias{units<-.errors}
\alias{set_units.quantities}
\alias{set_units.errors}
\alias{mixed_units.quantities}
\alias{mixed_units.errors}
\title{Handle Measurement Units on a Numeric Vector}
\usage{
\method{units}{quantities}(x) <- value

\method{units}{errors}(x) <- value

\method{set_units}{quantities}(x, value, ..., mode = units_options("set_units_mode"))

\method{set_units}{errors}(x, value, ..., mode = units_options("set_units_mode"))

\method{mixed_units}{quantities}(x, values, ...)

\method{mixed_units}{errors}(x, values, ...)
}
\arguments{
\item{x}{numeric vector, or object of class \code{units}.}

\item{value}{object of class \code{units} or \code{symbolic_units}, or in the
case of \code{set_units} expression with symbols (see examples).}

\item{...}{passed on to other methods.}

\item{mode}{if \code{"symbols"} (the default), then unit is constructed from
the expression supplied. Otherwise, if\code{mode = "standard"},
standard evaluation is used for the supplied value This argument can be set
via a global option \code{units_options(set_units_mode = "standard")}}

\item{values}{character vector with units encodings, or list with symbolic units of class \code{mixed_symbolic_units}}
}
\description{
Set or retrieve measurement units to/from numeric vectors and convert units
(extensions to the \pkg{units} package for \code{quantities} and
\code{errors} objects).
}
\seealso{
\code{\link[units]{units}}, \code{\link[units:units]{set_units}}.
}
