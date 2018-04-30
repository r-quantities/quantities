#include "parse.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector parse_vector_(CharacterVector x, char decimalMark, char groupingMark) {
  NumericVector val(x.size(), 0);
  NumericVector err(x.size(), 0);
  CharacterVector unt(x.size(), "1");
  CharacterVector::Proxy::iterator first, last;

  for (unsigned int i = 0; i < x.size(); i++) {
    first = x[i].begin();
    last = x[i].end();
    if (!parseErrors(first, last, val[i], err[i], decimalMark, groupingMark)) {
      val[i] = NA_REAL;
      err[i] = NA_REAL;
      unt[i] = unt[0];
    } else if (first != last)
      unt[i] = std::string(first, last);
  }

  val.attr("errors") = err;
  val.attr("units") = unt;
  return val;
}
