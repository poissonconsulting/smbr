pars_arg2to1 <- function(param_type, x, scalar) {
  pars(x = x, param_type = param_type, scalar = scalar)
}

error <- function(...) stop(..., call. = FALSE)
