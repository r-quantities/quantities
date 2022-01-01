test_that("basic parsing works", {
  expect_quantities(parse_quantities("a"), NA_real_, unitless, NA_real_)
  expect_quantities(parse_quantities("-1.234"), -1.234, unitless, 0)
  expect_quantities(parse_quantities("-.234"), -0.234, unitless, 0)
  expect_quantities(parse_quantities(".234"), 0.234, unitless, 0)
  expect_quantities(parse_quantities("1e+0"), 1, unitless, 0)
  expect_quantities(parse_quantities("1.234e+0"), 1.234, unitless, 0)
})

test_that("units are recognised", {
  expect_quantities(parse_quantities("1.234 m s-1"), 1.234, "m/s", 0)
  expect_quantities(parse_quantities("1.234(13) m s-1"), 1.234, "m/s", 0.013)
  expect_quantities(parse_quantities("-1.234(13)e+0 m s-1"), -1.234, "m/s", 0.013)
})

test_that("errors are recognised", {
  expect_quantities(parse_quantities("1(13)"), 1, unitless, 13)
  expect_quantities(parse_quantities("1(0.013)"), 1, unitless, 0.013)
  expect_quantities(parse_quantities("1+/-0.013"), 1, unitless, 0.013)
  expect_quantities(parse_quantities("1+-0.013"), 1, unitless, 0.013)
  expect_quantities(parse_quantities(paste0("1", intToUtf8(177), "0.013")), 1, unitless, 0.013)
  expect_quantities(parse_quantities("1.234(13)"), 1.234, unitless, 0.013)
  expect_quantities(parse_quantities("1.234(0.013)"), 1.234, unitless, 0.013)
  expect_quantities(parse_quantities("1.234+/-0.013"), 1.234, unitless, 0.013)
  expect_quantities(parse_quantities("1.234+-0.013"), 1.234, unitless, 0.013)
  expect_quantities(parse_quantities(
    paste0("1.234", intToUtf8(177), "0.013")), 1.234, unitless, 0.013)
})

test_that("errors and units are recognised but dropped", {
  expect_units(expect_warning(parse_units("1.234(13)")), 1.234, unitless)
  expect_units(expect_warning(parse_units("1.234+/-0.013")), 1.234, unitless)
  expect_units(expect_warning(parse_units("1.234(13) m/s")), 1.234, "m/s")
  expect_errors(expect_warning(parse_errors("1.234 m/s")), 1.234, 0)
  expect_errors(expect_warning(parse_errors("1.234(13) m/s")), 1.234, 0.013)
})

test_that("exponents are recognised", {
  expect_quantities(parse_quantities("1.234(13)e1"), 12.34, unitless, 0.13)
  expect_quantities(parse_quantities("1.234(13)e+1"), 12.34, unitless, 0.13)
  expect_quantities(parse_quantities("1.234(13)e-1"), 0.1234, unitless, 0.0013)
})

test_that("spaces are skipped", {
  expect_quantities(parse_quantities("  -1  e+0  m s-1"), -1, "m/s", 0)
  expect_quantities(parse_quantities("  -1  (  0.013  )  e+0  m s-1"), -1, "m/s", 0.013)
  expect_quantities(parse_quantities("  -1.234  (  13  )  e+0  m s-1"), -1.234, "m/s", 0.013)
  expect_quantities(parse_quantities("  -1.234  +/-  .013  )  e+0  m s-1"), -1.234, "m/s", 0.013)
  expect_quantities(parse_quantities(
    paste0("  -1.234  ", intToUtf8(177), "  .013  )  e+0  m s-1")), -1.234, "m/s", 0.013)
})

test_that("values are converted to the first unit seen, if possible", {
  expect_quantities(
    parse_quantities(c("1.234(13) m/s", "4.4424(468) km/h")),
    rep(1.234, 2), "m/s", rep(0.013, 2)
  )
  expect_units(
    parse_units(c("1.234 m/s", "4.4424 km/h")),
    rep(1.234, 2), "m/s"
  )

  x <- parse_quantities(c("1.234(13) m/s", "4.4424(468) km"))
  expect_s3_class(x, "mixed_units")
  expect_quantities(x[[1]], 1.234, "m/s", 0.013)
  expect_quantities(x[[2]], 4.4424, "km", 0.0468)
})
