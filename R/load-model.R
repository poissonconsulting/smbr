#' @export
load_model.smb_model <- function(x, quiet, ...) {
  check_flag(quiet)

  capture_output <- if (quiet) function(x) suppressWarnings(capture.output(x)) else eval

  capture_output(
    stanc <- rstan::stanc(model_code = template(x))
  )
  capture_output(
    stan_model <- rstan::stan_model(
      stanc_ret = stanc, save_dso = FALSE, auto_write = FALSE)
  )
  stan_model
}

#' @export
load_model.smb_analysis <- function(x, quiet, ...) {
  load_model(model(x), quiet, ...)
}
