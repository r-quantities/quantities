test_that("rounding methods work properly", {
  x <- set_quantities(c(-2, 0, 2), "m/s", 0.1)
  expect_equal(sign(x), c(-1, 0, 1))

  xval <- 1.1:10.1
  xerr <- seq(0.005, 0.05, 0.005)
  xunt <- "m/s"
  x <- set_quantities(xval, xunt, xerr, mode="standard")
  xu <- set_units(xval, xunt, mode="standard")
  xe <- set_errors(xval, xerr)

  expect_quantities(floor(x), floor(xval), units(floor(xu)), errors(floor(xe)))
  expect_quantities(ceiling(x), ceiling(xval), units(ceiling(xu)), errors(ceiling(xe)))
  expect_quantities(trunc(x), trunc(xval), units(trunc(xu)), errors(trunc(xe)))
  expect_quantities(round(x), round(xval), units(round(xu)), errors(round(xe)))
  expect_quantities(signif(x), signif(xval), units(signif(xu)), errors(signif(xe)))
})

test_that("math methods work properly", {
  xval <- 1.1:10.1/100
  xerr <- seq(0.005, 0.05, 0.005)/100
  xunt <- "m/s"
  x <- set_quantities(xval, xunt, xerr, mode="standard")
  xu <- set_units(xval, xunt, mode="standard")
  xe <- set_errors(xval, xerr)

  expect_error(sqrt(x))
  expect_quantities(sqrt(x^2), sqrt(xval^2), units(sqrt(xu^2)), errors(sqrt(xe^2)))
  expect_quantities(log(x), log(xval), units(log(xu)), errors(log(xe)))
  expect_quantities(log10(x), log(xval, 10), units(log(xu, 10)), errors(log(xe, 10)))
  expect_quantities(log2(x), log(xval, 2), units(log(xu, 2)), errors(log(xe, 2)))

  expect_warning(expect_errors(sin(x), sin(xval), errors(sin(xe))))
  expect_warning(expect_errors(cos(x), cos(xval), errors(cos(xe))))
  expect_warning(expect_errors(tan(x), tan(xval), errors(tan(xe))))
  expect_warning(expect_errors(asin(x), asin(xval), errors(asin(xe))))
  expect_warning(expect_errors(acos(x), acos(xval), errors(acos(xe))))
  expect_warning(expect_errors(atan(x), atan(xval), errors(atan(xe))))
  expect_warning(expect_errors(sinpi(x), sin(pi*xval), errors(sin(pi*xe))))
  expect_warning(expect_errors(cospi(x), cos(pi*xval), errors(cos(pi*xe))))
  expect_warning(expect_errors(tanpi(x), tan(pi*xval), errors(tan(pi*xe))))
  expect_warning(expect_errors(sinh(x), sinh(xval), errors(sinh(xe))))
  expect_warning(expect_errors(cosh(x), cosh(xval), errors(cosh(xe))))
  expect_warning(expect_errors(tanh(x), tanh(xval), errors(tanh(xe))))
  expect_warning(expect_errors(asinh(x), asinh(xval), errors(asinh(xe))))
  expect_warning(expect_errors(acosh(x*set_quantities(100)), acosh(xval*100), errors(acosh(xe*100))))
  expect_warning(expect_errors(atanh(x/set_quantities(100)), atanh(xval/100), errors(atanh(xe/100))))

  xunt <- "rad"
  x <- set_quantities(xval, xunt, xerr, mode="standard")
  xu <- set_units(xval, xunt, mode="standard")

  expect_quantities(sin(x), sin(xval), units(sin(xu)), errors(sin(xe)))
  expect_quantities(cos(x), cos(xval), units(cos(xu)), errors(cos(xe)))
  expect_quantities(tan(x), tan(xval), units(tan(xu)), errors(tan(xe)))
  expect_quantities(sinpi(x), sin(pi*xval), units(sin(pi*xu)), errors(sin(pi*xe)))
  expect_quantities(cospi(x), cos(pi*xval), units(cos(pi*xu)), errors(cos(pi*xe)))
  expect_quantities(tanpi(x), tan(pi*xval), units(tan(pi*xu)), errors(tan(pi*xe)))

  xunt <- "1"
  x <- set_quantities(xval, xunt, xerr, mode="standard")
  xu <- set_units(xval, xunt, mode="standard")

  expect_quantities(asin(x), asin(xval), units(asin(xu)), errors(asin(xe)))
  expect_quantities(acos(x), acos(xval), units(acos(xu)), errors(acos(xe)))
  expect_quantities(atan(x), atan(xval), units(atan(xu)), errors(atan(xe)))
})

test_that("cumulative methods work properly", {
  xval <- 1.1:10.1/100
  xerr <- seq(0.005, 0.05, 0.005)/100
  xunt <- "m/s"
  x <- set_quantities(xval, xunt, xerr, mode="standard")
  xu <- set_units(xval, xunt, mode="standard")
  xe <- set_errors(xval, xerr)

  expect_quantities(cumsum(x), cumsum(xval), units(cumsum(xu)), errors(cumsum(xe)))
  expect_warning(expect_errors(cumprod(x), cumprod(xval), errors(cumprod(xe))))
  expect_quantities(cummax(x), cummax(xval), units(cummax(xu)), errors(cummax(xe)))
  expect_quantities(cummin(x), cummin(xval), units(cummin(xu)), errors(cummin(xe)))
})
