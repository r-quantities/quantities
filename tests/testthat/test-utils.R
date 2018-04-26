context("utils")

test_that("objects with errors & units attributes are reclassed", {
  x <- 1
  expect_is(reclass(x), "numeric")
  attr(x, "errors") <- 1
  attr(x, "units") <- NULL
  expect_is(reclass(x), "numeric")
  attr(x, "errors") <- NULL
  attr(x, "units") <- 1
  expect_is(reclass(x), "numeric")
  attr(x, "errors") <- 1
  attr(x, "units") <- 1
  expect_is(reclass(x), "quantities")
})

test_that("offset units (vs. scale units) are detected", {
  expect_true(is_offset(as_units("K"), as_units("celsius")))
  expect_false(is_offset(as_units("K"), as_units("mK")))
})

test_that("dots are converted to the units of the first argument", {
  xval <- 1
  xerr <- 0.1
  x <- set_quantities(xval, m/s, xerr)
  y <- set_units(x, km/h)
  z <- set_quantities(xval, m, xerr)

  expect_quantities(cbind(x, y, x, y), rep(xval, 4), units(as_units("m/s")), rep(xerr, 4))
  expect_error(cbind(x, 2))
  expect_error(cbind(x, z))
})
