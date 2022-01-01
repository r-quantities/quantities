options(errors.warn.bool = FALSE)
options(errors.warn.coercion = FALSE)
options(errors.warn.matmult = FALSE)

expect_quantities <- function(x, xval, xunt, xerr) {
  expect_equal(class(x), c("quantities", "units", "errors"))
  expect_equal(as.numeric(x), xval)
  expect_equal(as.character(attr(x, "units")), as.character(xunt))
  expect_equal(attr(x, "errors"), xerr)
}

expect_errors <- function(x, xval, xerr) {
  expect_equal(class(x), "errors")
  expect_equal(as.numeric(x), xval)
  expect_equal(attr(x, "errors"), xerr)
}

expect_units <- function(x, xval, xunt) {
  expect_equal(class(x), "units")
  expect_equal(as.numeric(x), xval)
  expect_equal(as.character(attr(x, "units")), as.character(xunt))
}
