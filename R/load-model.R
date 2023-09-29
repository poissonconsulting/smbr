#' @export
load_model.smb_model <- function(x, quiet, ...) {
  chk_flag(quiet)

  capture_output <- if (quiet) function(x) suppressWarnings(capture.output(x)) else eval

  capture_output(
    stanc <- rstan::stanc(model_code = template(x))
  )
  capture_output(
    stan_model <- rstan::stan_model(
      stanc_ret = stanc, save_dso = FALSE, auto_write = FALSE
    )
  )
  stan_model
}
