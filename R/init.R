
.onLoad = function(libname, pkgname) {
  register_all_s3_methods()

  # geom_errors() compatibility with scale_[x|y]_units()
  options(errors.compute_statistic = function(data, layout) {
    scales <- layout$get_scales(data$PANEL[1])
    if (!is.null(data$xmin_errors)) {
      units(data$xmin_errors) <- scales$x$units
      units(data$xmax_errors) <- scales$x$units
      data$xmin_errors <- as.numeric(data$xmin_errors)
      data$xmax_errors <- as.numeric(data$xmax_errors)
    }
    if (!is.null(data$ymin_errors)) {
      units(data$ymin_errors) <- scales$y$units
      units(data$ymax_errors) <- scales$y$units
      data$ymin_errors <- as.numeric(data$ymin_errors)
      data$ymax_errors <- as.numeric(data$ymax_errors)
    }
    data
  })
}
