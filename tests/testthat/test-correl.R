test_that("wrong values fail", {
  xval <- -4.1:5.1
  xerr <- seq(0.005, 0.05, 0.005)
  xunt <- "m/s"
  x <- set_quantities(xval, xunt, xerr, mode="standard")
  y <- set_quantities(xval, xunt, xerr, mode="standard")

  # non-errors y
  expect_error(covar(x, 1))
  expect_error(correl(x, 1))
  expect_error(covar(x, 1) <- 0)
  expect_error(correl(x, 1) <- 0)

  # wrong lengths
  expect_error(covar(x, set_errors(1)) <- 0)
  expect_error(correl(x, set_errors(1)) <- 0)
  expect_error(covar(x, set_errors(rep(1, 2))) <- 0)
  expect_error(correl(x, set_errros(rep(1, 2))) <- 0)

  # cannot modify self-correlation (same id)
  expect_error(covar(x, x) <- 0)
  expect_error(correl(x, x) <- 0)

  # wrong range
  expect_error(covar(x, y) <- 1e6)
  expect_error(correl(x, y) <- -2)
})

test_that("covariances are correctly stored, retrieved and removed", {
  x <- set_quantities(1:10, m/s, 0.1)
  y <- set_quantities(10:1, km/h, 0.2)
  z <- x

  expect_equal(covar(x, x), errors(x)^2)
  expect_equal(covar(y, y), errors(y)^2)
  expect_null(covar(x, y))
  expect_null(correl(x, y))

  expect_error(correl(x, y) <- set_units(0.3, m/s))
  correl(x, y) <- set_units(0.3)

  expect_equal(correl(x, y), rep(set_units(0.3), length(x)))
  expect_equal(correl(z, y), rep(set_units(0.3), length(x)))
  expect_equal(covar(x, y), 0.3 * errors(x) * errors(y))
  expect_equal(covar(z, y), 0.3 * errors(x) * errors(y))

  expect_error(covar(x, y) <- 0.003)
  covar(x, y) <- set_units(0.003, m^2/s^2)

  expect_equal(correl(x, y), rep(set_units(0.54), length(x)))
  expect_equal(covar(x, y), 0.54 * errors(x) * errors(y))
})

test_that("pipe-friendly versions work as expected", {
  x <- set_quantities(1:10, m/s, 0.1)
  y <- set_quantities(10:1, km/h, 0.2)

  expect_identical(set_correl(x, y, 0.3), x)
  expect_equal(covar(x, y), 0.3 * errors(x) * errors(y))
  expect_identical(set_correl(y, x, 0.35), y)
  expect_equal(covar(x, y), 0.35 * errors(x) * errors(y))
  expect_identical(set_covar(x, y, set_units(0.003, m^2/s^2)), x)
  expect_equal(correl(x, y), set_units(0.003, m^2/s^2) / errors(x) / errors(y))
  expect_identical(set_covar(y, x, set_units(0.0035, m^2/s^2)), y)
  expect_equal(correl(x, y), set_units(0.0035, m^2/s^2) / errors(x) / errors(y))
})
