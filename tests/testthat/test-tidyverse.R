test_that("pillar methods work for errors objects", {
  skip_if_not_installed("pillar")

  x <- set_quantities(1, "m", 0.1)

  expect_equal(unclass(pillar::type_sum(x)), "(err) [m]")
  expect_s3_class(pillar::type_sum(x), "type_sum_errors")
  expect_equal(as.character(pillar::pillar_shaft(x)),
               as.character(pillar::pillar_shaft(drop_units(x))))
})

test_that("can proxy and restore quantities", {
  skip_if_not_installed("vctrs", "0.3.1")

  x <- set_quantities(1:3, "cm", 3:1, mode = "standard")
  out <- vctrs::vec_restore(vctrs::vec_proxy(x), x)
  expect_equal(out, x)
})

test_that("can slice quantities", {
  skip_if_not_installed("vctrs", "0.3.1")
  skip_if_not_installed("dplyr", "1.0.0")

  x <- set_quantities(1:3, "cm", 3:1, mode = "standard")
  exp <- list(x[1], x[2], x[3])
  expect_equal(vctrs::vec_chop(x), exp)

  df <- dplyr::tibble(x = dplyr::tibble(x = x))
  exp <- list(
    dplyr::tibble(x = dplyr::tibble(x = x[1])),
    dplyr::tibble(x = dplyr::tibble(x = x[2])),
    dplyr::tibble(x = dplyr::tibble(x = x[3]))
  )
  expect_equal(vctrs::vec_chop(df), exp)
})

test_that("can combine quantities", {
  skip_if_not_installed("vctrs", "0.3.1")
  skip_if_not_installed("dplyr", "1.0.0")

  x <- set_quantities(1:3, "cm", 3:1, mode = "standard")
  df <- dplyr::tibble(x = dplyr::tibble(x = x))

  out <- vctrs::list_unchop(vctrs::vec_chop(x))
  expect_equal(out, x)

  out <- vctrs::list_unchop(vctrs::vec_chop(df))
  expect_equal(out, df)
})

test_that("quantities have coercion methods", {
  skip_if_not_installed("vctrs", "0.3.1")

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
})

test_that("split-apply-combine with dplyr and base agree", {
  skip_if_not_installed("vctrs", "0.3.1")
  skip_if_not_installed("dplyr", "1.0.0")

  `%>%` <- dplyr::`%>%`
  iris2 <- iris
  for (i in 1:4)
    quantities(iris2[,i]) <- list("cm", iris2[,i] * 0.05)

  out <- iris2 %>%
    dplyr::group_by(Species) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), mean))

  # Transform to list of lists
  out <- vctrs::vec_chop(out[2:5]) %>%
    stats::setNames(out$Species) %>%
    lapply(as.list)

  exp <- lapply(split(iris2[1:4], iris2$Species), lapply, mean)
  expect_equal(out, exp)
})
