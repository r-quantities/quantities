test_that("summary methods work properly", {
  xval <- c(-1, 0, 1)
  xerr <- c(0.2, 0.1, 0.18)
  xunt <- "m/s"
  x <- set_quantities(xval, xunt, xerr, mode="standard")
  xu <- set_units(xval, xunt, mode="standard")
  xe <- set_errors(xval, xerr)

  # all, any not allowed by units nor errors
  # prod not allowed by units
  expect_units(max(x), max(xval + xerr), xunt)
  expect_units(min(x), min(xval - xerr), xunt)
  expect_units(range(x), c(min(xval - xerr), max(xval + xerr)), xunt)
  expect_quantities(sum(x), sum(xval), units(sum(xu)), errors(sum(xe)))
  expect_quantities(mean(x), mean(xval), units(mean(xu)), errors(mean(xe)))
  expect_quantities(weighted.mean(x), weighted.mean(xval), units(weighted.mean(xu)), errors(weighted.mean(xe)))
  expect_quantities(median(x), median(xval), units(median(xu)), errors(median(xe)))
})
