
test_that("can print quantities in tibble", {
  skip_if_not_installed("tibble")

  verify_output(test_path("output", "tibble-print.txt"), {
    x <- set_quantities(1:3, "cm", 3:1, mode = "standard")

    x
    tibble::tibble(x = x)
    tibble::tibble(tibble::tibble(x = x))
  })
})


skip_if_not_installed("vctrs")

test_that("can proxy and restore quantities", {
  x <- set_quantities(1:3, "cm", 3:1, mode = "standard")
  out <- vctrs::vec_restore(vctrs::vec_proxy(x), x)
  expect_equal(out, x)
})

test_that("can slice quantities", {
  x <- set_quantities(1:3, "cm", 3:1, mode = "standard")
  exp <- list(x[1], x[2], x[3])
  expect_equal(vctrs::vec_chop(x), exp)

  df <- tibble::tibble(x = tibble::tibble(x = x))
  exp <- list(
    tibble::tibble(x = tibble::tibble(x = x[1])),
    tibble::tibble(x = tibble::tibble(x = x[2])),
    tibble::tibble(x = tibble::tibble(x = x[3]))
  )
  expect_equal(vctrs::vec_chop(df), exp)
})

test_that("can combine quantities", {
  x <- set_quantities(1:3, "cm", 3:1, mode = "standard")
  df <- tibble::tibble(x = tibble::tibble(x = x))

  out <- vctrs::vec_unchop(vctrs::vec_chop(x))
  expect_equal(out, x)

  out <- vctrs::vec_unchop(vctrs::vec_chop(df))
  expect_equal(out, df)
})

test_that("quantities have coercion methods", {
  x <- set_quantities(1:3, "cm", 3:1, mode = "standard")
  y <- set_quantities(4, "m", 2, mode = "standard")
  z <- set_quantities(10, "celsius", 20, mode = "standard")

  expect_error(
    vctrs::vec_ptype_common(x, y, z),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    vctrs::vec_cast_common(z, x, y),
    class = "vctrs_error_incompatible_type"
  )

  out <- vctrs::vec_ptype_common(x, y)
  expect_equal(out, set_quantities(double(), "cm", mode = "standard"))
  expect_equal(typeof(out), "double")

  out <- vctrs::vec_ptype_common(y, x)
  expect_equal(out, set_quantities(double(), "m", mode = "standard"))
  expect_equal(typeof(out), "double")

  out <- vctrs::vec_cast_common(y, x)
  exp <- list(
    set_quantities(4, "m", 2, mode = "standard"),
    set_quantities(c(0.01, 0.02, 0.03), "m", c(0.03, 0.02, 0.01), mode = "standard")
  )
  expect_equal(out, exp)
  expect_equal(sapply(out, typeof), c("double", "double"))

  out <- vctrs::vec_cast_common(x, y)
  exp <- list(
    set_quantities(as.double(1:3), "cm", as.double(3:1), mode = "standard"),
    set_quantities(400, "cm", 200, mode = "standard")
  )
  expect_equal(sapply(out, typeof), c("double", "double"))

  expect_error(
    vctrs::vec_cast(x, set_quantities(0L, "m")),
    class = "vctrs_error_cast_lossy"
  )
})
