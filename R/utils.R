reclass <- function(x) {
  class(x) <- c("quantities", "units", "errors")
  x
}

.convert_to_first_arg <- function(dots, env.=parent.frame()) {
  dots <- deparse(substitute(dots))
  modified <- FALSE
  u <- units(env.[[dots]][[1]])
  for (i in seq_along(env.[[dots]])[-1]) {
    if (!inherits(env.[[dots]][[i]], "units"))
      stop(paste("argument", i, "is not of class units"))
    if (units(env.[[dots]][[i]]) == u)
      next
    if (class(try(units(env.[[dots]][[i]]) <- u)) == "try-error")
      stop(paste("argument", i,
                 "has units that are not convertible to that of the first argument"))
    units(env.[[dots]][[i]]) <- u
    modified <- TRUE
  }
  modified
}
