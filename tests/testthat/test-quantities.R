test_that("quantities objects are correctly created (standard)", {
  xval <- c(-1, 0, 1)
  xerr <- c(0.2, 0.1, 0.18)
  xunt <- "m/s"

  x <- xval
  quantities(x) <- list(xunt, xerr)
  expect_quantities(x, xval, xunt, xerr)

  x <- set_quantities(xval, xunt, xerr, mode="standard")
  expect_quantities(x, xval, xunt, xerr)
  x <- set_quantities(xval, m/s, xerr)
  expect_quantities(x, xval, xunt, xerr)

  x <- set_units(xval, xunt, mode="standard")
  quantities(x) <- list(xunt, xerr)
  expect_quantities(x, xval, xunt, xerr)

  x <- set_units(xval, xunt, mode="standard")
  x <- set_quantities(x, xunt, xerr, mode="standard")
  expect_quantities(x, xval, xunt, xerr)
  x <- set_units(xval, m/s)
  x <- set_quantities(x, m/s, xerr)
  expect_quantities(x, xval, xunt, xerr)

  x <- set_units(xval, xunt, mode="standard")
  errors(x) <- set_units(xerr, xunt, mode="standard")
  expect_quantities(x, xval, xunt, xerr)

  x <- set_units(xval, xunt, mode="standard")
  x <- set_errors(x, set_units(xerr, xunt, mode="standard"))
  expect_quantities(x, xval, xunt, xerr)

  x <- set_errors(xval, xerr)
  quantities(x) <- list(xunt, xerr)
  expect_quantities(x, xval, xunt, xerr)

  x <- set_errors(xval, xerr)
  x <- set_quantities(x, xunt, xerr, mode="standard")
  expect_quantities(x, xval, xunt, xerr)
  x <- set_errors(xval, xerr)
  x <- set_quantities(x, m/s, xerr)
  expect_quantities(x, xval, xunt, xerr)

  x <- set_errors(xval, xerr)
  units(x) <- xunt
  expect_quantities(x, xval, xunt, xerr)

  x <- set_errors(xval, xerr)
  x <- set_units(x, xunt, mode="standard")
  expect_quantities(x, xval, xunt, xerr)
  x <- set_errors(xval, xerr)
  x <- set_units(x, m/s)
  expect_quantities(x, xval, xunt, xerr)
})

test_that("units and errors can be converted", {
  xval <- c(-1, 0, 1)
  xerr <- c(0.2, 0.1, 0.18)
  xunt <- "m/s"
  xunt_conv <- "km/h"

  x <- set_quantities(xval, xunt, xerr, mode="standard")
  expect_error(quantities(x) <- list("l", 2 * xerr))
  quantities(x) <- list(xunt_conv, 2 * xerr)
  expect_quantities(x, 3.6 * xval, xunt_conv, 2 * xerr)

  x <- set_quantities(xval, xunt, xerr, mode="standard")
  expect_error(set_quantities(x, "l", 2 * xerr))
  x <- set_quantities(x, xunt_conv, 2 * xerr, mode="standard")
  expect_quantities(x, 3.6 * xval, xunt_conv, 2 * xerr)

  x <- set_quantities(xval, xunt, xerr, mode="standard")
  expect_error(units(x) <- "l")
  units(x) <- xunt_conv
  expect_quantities(x, 3.6 * xval, xunt_conv, 3.6 * xerr)
  errors(x) <- set_units(2 * xerr, xunt_conv, mode="standard")
  expect_quantities(x, 3.6 * xval, xunt_conv, 2 * xerr)

  x <- set_quantities(xval, xunt, xerr, mode="standard")
  expect_error(set_units(x, "l"))
  x <- set_units(x, xunt_conv, mode="standard")
  expect_quantities(x, 3.6 * xval, xunt_conv, 3.6 * xerr)
  x <- set_errors(x,set_units(2 * xerr, xunt_conv, mode="standard"))
  expect_quantities(x, 3.6 * xval, xunt_conv, 2 * xerr)

  x <- set_quantities(xval, xunt, xerr, mode="standard")
  xmax <- errors_max(x)
  xmin <- errors_min(x)
  expect_units(xmax, xval + xerr, xunt)
  expect_units(xmin, xval - xerr, xunt)
})

test_that("units and errors can be dropped", {
  xval <- c(-1, 0, 1)
  xerr <- c(0.2, 0.1, 0.18)
  xunt <- "m/s"

  x <- set_quantities(xval, xunt, xerr, mode="standard")
  quantities(x) <- NULL
  expect_equal(x, xval)

  x <- set_quantities(xval, xunt, xerr, mode="standard")
  x <- set_quantities(x, NULL, NULL)
  expect_equal(x, xval)

  x <- set_quantities(xval, xunt, xerr, mode="standard")
  x <- drop_quantities(x)
  expect_equal(x, xval)

  x <- set_quantities(xval, xunt, xerr, mode="standard")
  units(x) <- NULL
  expect_errors(x, xval, xerr)

  x <- set_quantities(xval, xunt, xerr, mode="standard")
  x <- set_units(x, NULL)
  expect_errors(x, xval, xerr)

  x <- set_quantities(xval, xunt, xerr, mode="standard")
  x <- drop_units(x)
  expect_errors(x, xval, xerr)

  x <- set_quantities(xval, xunt, xerr, mode="standard")
  errors(x) <- NULL
  expect_units(x, xval, xunt)

  x <- set_quantities(xval, xunt, xerr, mode="standard")
  x <- set_errors(x, NULL)
  expect_units(x, xval, xunt)

  x <- set_quantities(xval, xunt, xerr, mode="standard")
  x <- drop_errors(x)
  expect_units(x, xval, xunt)

  ox <- x <- data.frame(x=1:4, y=1:4)
  quantities(x[[1]]) <- list("m/s", 0.1)
  expect_s3_class(x[[1]], "quantities")
  expect_identical(drop_quantities(x), ox)
})

test_that("units and errors are correctly retrieved", {
  xval <- c(-1, 0, 1)
  xerr <- c(0.2, 0.1, 0.18)
  xunt <- "m/s"

  x <- set_quantities(xval, xunt, xerr, mode="standard")
  expect_equal(quantities(x)[["units"]], units(as_units(xunt)))
  expect_equal(units(x), units(as_units(xunt)))
  expect_equal(quantities(x)[["errors"]], xerr)
  expect_equal(drop_units(errors(x)), xerr)
})

test_that("defaults work as expected", {
  xval <- c(0, NA, NaN, Inf)
  x <- set_quantities(xval)
  expect_equal(units(x), units(as_units("1")))
  expect_equal(as.numeric(x), xval)
  expect_equal(drop_units(errors(x)), xval)
})

test_that("a zero-valued non-offset quantity DOES scale errors", {
  x <- set_units(set_quantities(0, "m", 1), "km")
  expect_quantities(x, 0, "km", 0.001)
})

test_that("unit conversion of errors works correctly", {
  xval <- 0:1
  xerr <- 0.2

  x <- set_errors(set_quantities(xval, "m", xerr), set_units(1, "mm"))
  y <- set_units(set_quantities(xval, "m", xerr), "mm")
  yval <- as.numeric(set_units(set_units(xval, "m"), "mm"))
  expect_quantities(x, xval, "m", rep(1/diff(yval), 2))
  expect_quantities(y, yval, "mm", rep(xerr*diff(yval), 2))

  x <- set_errors(set_quantities(xval, "celsius", xerr), set_units(1, "K"))
  y <- set_units(set_quantities(xval, "celsius", xerr), "K")
  yval <- as.numeric(set_units(set_units(xval, "celsius"), "K"))
  expect_quantities(x, xval, "celsius", rep(1/diff(yval), 2))
  expect_quantities(y, yval, "K", rep(xerr*diff(yval), 2))

  x <- set_errors(set_quantities(xval, "celsius", xerr), set_units(1, "Fahrenheit"))
  y <- set_units(set_quantities(xval, "celsius", xerr), "Fahrenheit")
  yval <- as.numeric(set_units(set_units(xval, "celsius"), "Fahrenheit"))
  expect_quantities(x, xval, "celsius", rep(1/diff(yval), 2))
  expect_quantities(y, yval, "Fahrenheit", rep(xerr*diff(yval), 2))
})
