
test_that("can print quantities in tibble", {
  skip_if_not_installed("tibble")

  verify_output(test_path("output", "tibble-print.txt"), {
    x <- set_quantities(1:3, "cm", 3:1, mode = "standard")

    x
    tibble::tibble(x = x)
    tibble::tibble(tibble::tibble(x = x))
  })
})
