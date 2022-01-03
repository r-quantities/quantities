test_that("base plots work as expected", suppressWarnings({
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("errors", "0.3.6.1")
  skip_if_not_installed("units", "0.8-0")
  fplot <- function(...) function() plot(...)

  iris.q <- iris
  for (i in 1:4)
    quantities(iris.q[,i]) <- list("cm", iris.q[,i] * 0.02)

  vdiffr::expect_doppelganger("plot dataframe", fplot(
    iris.q[, c("Sepal.Length", "Sepal.Width")], col=iris.q$Species))

  vdiffr::expect_doppelganger("plot x", with(
    iris.q, fplot(Sepal.Width, col=Species)))
  vdiffr::expect_doppelganger("plot x drop units", with(
    iris.q, fplot(drop_units(Sepal.Width), col=Species)))
  vdiffr::expect_doppelganger("plot x drop errors", with(
    iris.q, fplot(drop_errors(Sepal.Width), col=Species)))

  vdiffr::expect_doppelganger("plot xy", with(
    iris.q, fplot(Sepal.Length, Sepal.Width, col=Species)))
  vdiffr::expect_doppelganger("plot xy drop units x", with(
    iris.q, fplot(drop_units(Sepal.Length), Sepal.Width, col=Species)))
  vdiffr::expect_doppelganger("plot xy drop units y", with(
    iris.q, fplot(Sepal.Length, drop_units(Sepal.Width), col=Species)))
  vdiffr::expect_doppelganger("plot xy drop errors x", with(
    iris.q, fplot(drop_errors(Sepal.Length), Sepal.Width, col=Species)))
  vdiffr::expect_doppelganger("plot xy drop errors y", with(
    iris.q, fplot(Sepal.Length, drop_errors(Sepal.Width), col=Species)))
  vdiffr::expect_doppelganger("plot xy drop quantities y", with(
    iris.q, fplot(Sepal.Length, drop_quantities(Sepal.Width), col=Species)))
}))

test_that("ggplot2 plots work as expected", suppressWarnings({
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("errors", "0.3.6.1")
  skip_if_not_installed("units", "0.8-0")
  library(ggplot2)

  iris.q <- iris
  for (i in 1:4)
    quantities(iris.q[,i]) <- list("cm", iris.q[,i] * 0.02)

  p0 <- ggplot(iris.q) + aes(Sepal.Length, Sepal.Width, color=Species) +
    geom_point() + theme_bw() + theme(legend.position=c(0.6, 0.8))
  p1 <- p0 +
    geom_errorbar(aes(ymin=errors_min(Sepal.Width), ymax=errors_max(Sepal.Width))) +
    geom_errorbarh(aes(xmin=errors_min(Sepal.Length), xmax=errors_max(Sepal.Length)))
  p2 <- p0 + geom_errors()
  p3 <- p0 + geom_errors(aes(x=drop_errors(Sepal.Length)))
  p4 <- p0 + geom_errors(aes(y=drop_errors(Sepal.Width)))
  p5 <- p1 + scale_x_units(unit="mm") + scale_y_units(unit="m")
  p6 <- p2 + scale_x_units(unit="mm") + scale_y_units(unit="m")
  p7 <- p3 + scale_x_units(unit="mm") + scale_y_units(unit="m")
  p8 <- p4 + scale_x_units(unit="mm") + scale_y_units(unit="m")

  vdiffr::expect_doppelganger("ggplot2 explicit", p1)
  vdiffr::expect_doppelganger("ggplot2 automatic", p2)
  vdiffr::expect_doppelganger("ggplot2 y", p3)
  vdiffr::expect_doppelganger("ggplot2 x", p4)
  vdiffr::expect_doppelganger("ggplot2 explicit scale", p5)
  vdiffr::expect_doppelganger("ggplot2 automatic scale", p6)
  vdiffr::expect_doppelganger("ggplot2 y scale", p7)
  vdiffr::expect_doppelganger("ggplot2 x scale", p8)
}))
