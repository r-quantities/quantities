context("misc")

test_that("subsetting methods work properly", {
  xval <- 1.1:10.1
  xerr <- seq(0.005, 0.05, 0.005)
  xunt <- "m/s"
  x <- set_quantities(xval, xunt, xerr, mode="standard")

  expect_equal(x[[3]], x[3])
  expect_quantities(x[[3]], xval[[3]], xunt, xerr[[3]])
  expect_quantities(x[c(3, 5)], xval[c(3, 5)], xunt, xerr[c(3, 5)])
  x[c(3, 5)] <- x[c(5, 3)]
  x[[4]] <- x[[10]]
  expect_quantities(x[3:5], xval[c(5, 10, 3)], xunt, xerr[c(5, 10, 3)])
})

test_that("concatenation methods work properly", {
  xval <- 1.1:10.1
  xerr <- seq(0.005, 0.05, 0.005)
  xunt <- "m/s"
  x <- set_quantities(xval, xunt, xerr, mode="standard")

  expect_quantities(c(c(x, x), x), rep(xval, 3), xunt, rep(xerr, 3))
  expect_quantities(rep(x, 3), rep(xval, 3), xunt, rep(xerr, 3))
})

test_that("diff method works properly", {
  xval <- 1.1:10.1
  xerr <- seq(0.005, 0.05, 0.005)
  xunt <- "m/s"
  x <- set_quantities(xval, xunt, xerr, mode="standard")

  y <- x
  for (i in 1:(length(y)-1)) y[i] <- y[i+1] - y[i]
  expect_quantities(diff(x), diff(xval), xunt, errors(y)[1:9])
})

test_that("matrix methods work properly", {
  xval <- 1:6
  xerr <- xval
  xunt <- "m/s"
  x <- set_quantities(xval, xunt, xerr, mode="standard")
  xm <- as.matrix(x)

  expect_equal(dim(xm), c(6, 1))
  expect_quantities(xm, xval, xunt, xerr)


  xm <- set_quantities(cbind(1:3, 4:6), xunt, xerr, mode="standard")

  expect_equal(dim(xm), c(3, 2))
  expect_quantities(xm, xval, xunt, xerr)

  xval <- c(1, 4, 2, 5, 3, 6)
  xerr <- xval
  expect_quantities(t(xm), xval, xunt, xerr)
})

test_that("data frame coercion works properly", {
  xval <- 1:10
  xerr <- xval
  xunt <- "m/s"
  x <- set_quantities(xval, xunt, xerr, mode="standard")
  y <- rev(x)

  df <- as.data.frame(cbind(x, y))
  expect_quantities(df$x, xval, xunt, xerr)
  expect_quantities(df$y, rev(xval), xunt, rev(xerr))

  df <- as.data.frame(rbind(x, y))
  expect_quantities(df[1, 1], xval[1], xunt, xerr[1])
  expect_quantities(df[2, 1], xval[10], xunt, xerr[10])
  expect_equal(rownames(df), c("x", "y"))

  df <- data.frame(xval, x)
  expect_quantities(df$x, xval, xunt, xerr)

  df <- cbind(df, xval, data.frame(x))
  expect_equal(df[[3]], xval)
  expect_quantities(df[[4]], xval, xunt, xerr)

  df <- rbind(df, xval[1:4], df[1,])
  expect_equal(df[[1]], c(xval, 1, 1))
  expect_quantities(df[[2]], c(xval, 2, 1), xunt, c(xerr, 0, 1))
  expect_equal(df[[3]], c(xval, 3, 1))
  expect_quantities(df[[4]], c(xval, 4, 1), xunt, c(xerr, 0, 1))
})

test_that("list coercion works properly", {
  x <- set_quantities(1:10, m, 10:1)
  y <- as.list(x)
  expect_is(y, "list")
  expect_true(all(sapply(seq_along(y), function(i) all.equal(y[[i]], x[i]))))
})

test_that("bind methods work properly", {
  xval <- 1:10
  xerr <- xval
  xunt <- "m/s"
  a <- set_quantities(xval, xunt, xerr, mode="standard")

  x <- rbind(x=a, y=a)
  expect_quantities(x, rep(xval, each=2), xunt, rep(xerr, each=2))
  expect_equal(rownames(x), c("x", "y"))

  x <- rbind(rbind(a, a), a)
  expect_quantities(x, rep(xval, each=3), xunt, rep(xerr, each=3))
  expect_equal(rownames(x), c("a", "a", "a"))

  x <- rbind(a, rbind(a, a))
  expect_quantities(x, rep(xval, each=3), xunt, rep(xerr, each=3))
  expect_equal(rownames(x), c("a", "a", "a"))

  x <- cbind(x=a, y=a)
  expect_quantities(x, rep(xval, 2), xunt, rep(xval, 2))
  expect_equal(colnames(x), c("x", "y"))

  x <- cbind(cbind(a, a), a)
  expect_quantities(x, rep(xval, 3), xunt, rep(xval, 3))
  expect_equal(colnames(x), c("a", "a", "a"))

  x <- cbind(a, cbind(a, a))
  expect_quantities(x, rep(xval, 3), xunt, rep(xval, 3))
  expect_equal(colnames(x), c("a", "a", "a"))
})

test_that("type_sum is available for quantities objects", {
  skip_if_not_installed("tibble")
  library(tibble)
  expect_equal(type_sum(set_quantities(1, "m", 0.1)), "[(err) m]")
})

test_that("pillar_shaft is available for quantities objects", {
  skip_if_not_installed("pillar")
  library(pillar)
  expect_equal(as.character(pillar_shaft(set_quantities(1, "m", 0.1))),
               paste0("1.0", style_subtle("(1)"), " ", style_subtle("m")))
})
