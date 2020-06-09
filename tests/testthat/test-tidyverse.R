
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
