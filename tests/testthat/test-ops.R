context("ops")

test_that("arithmetic operators work properly", {
  xval <- 1.1:10.1/100
  yval <- xval - 0.001
  xerr <- seq(0.005, 0.05, 0.005)/100
  xunt <- "m/s"
  x <- set_quantities(xval, xunt, xerr, mode="standard")
  xu <- set_units(xval, xunt, mode="standard")
  xe <- set_errors(xval, xerr)
  y <- set_quantities(yval, xunt, xerr, mode="standard")
  yu <- set_units(yval, xunt, mode="standard")
  ye <- set_errors(yval, xerr)

  expect_quantities(+x, +xval, units(+xu), errors(+xe))
  expect_quantities(-x, -xval, units(-xu), errors(-xe))
  expect_quantities(x + y, xval + yval, units(xu + yu), errors(xe + ye))
  expect_quantities(x - y, xval - yval, units(xu - yu), errors(xe - ye))
  expect_quantities(x * y, xval * yval, units(xu * yu), errors(xe * ye))
  expect_quantities(x / y, xval / yval, units(xu / yu), errors(xe / ye))
  expect_error(x ^ y)
  expect_quantities(x %% y, xval %% yval, units(xu %% yu), errors(xe %% ye))
  expect_quantities(x %/% y, xval %/% yval, units(xu %/% yu), errors(xe %/% ye))
})

test_that("logical operators work as expected", {
  xval <- 1.1:10.1/100
  yval <- xval - 0.001
  xerr <- seq(0.005, 0.05, 0.005)/100
  xunt <- "m/s"
  x <- set_quantities(xval, xunt, xerr, mode="standard")
  xu <- set_units(xval, xunt, mode="standard")
  xe <- set_errors(xval, xerr)
  y <- set_quantities(yval, xunt, xerr, mode="standard")
  yu <- set_units(yval, xunt, mode="standard")
  ye <- set_errors(yval, xerr)

  expect_equal(!x, !xval)
  expect_error(x & y)
  expect_error(x | y)
  expect_equal(x == y, xval == yval)
  expect_equal(x != y, xval != yval)
  expect_equal(x < y, xval < yval)
  expect_equal(x <= y, xval <= yval)
  expect_equal(x >= y, xval >= yval)
  expect_equal(x > y, xval > yval)
})

test_that("simplification works as expected", {
  simplify.old <- units_options("simplify")
  on.exit(units_options(simplify=simplify.old))
  units_options(simplify=TRUE)

  xval <- 1100
  yval <- 1.1
  xerr <- 100
  yerr <- 0.1

  x <- set_quantities(xval, "mm", xerr) * set_quantities(yval, "m^-1", yerr)
  xu <- set_units(xval, "mm") * set_units(yval, "m^-1")
  xe <- set_errors(xval, xerr) * set_errors(yval, yerr) *
    set_errors(as.numeric(xu) / (xval*yval), 0)

  expect_quantities(x, as.numeric(xu), units(xu), errors(xe))

  x <- set_quantities(xval, "mm", xerr) / set_quantities(yval, "m", yerr)
  xu <- set_units(xval, "mm") / set_units(yval, "m")
  xe <- set_errors(xval, xerr) / set_errors(yval, yerr) *
    set_errors(as.numeric(xu) / (xval/yval), 0)

  expect_quantities(x, as.numeric(xu), units(xu), errors(xe))
})
