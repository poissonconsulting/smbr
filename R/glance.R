#' @export
glance.smb_analysis <- function(x, n = NULL, rhat = getOption("mb.rhat", 1.1), esr = getOption("mb.esr", 0.33), ...) {
  check_number(rhat)

  rhat_analysis <- rhat(x)
  rhat_arg <- rhat

  esr_analysis <- esr(x)
  esr_arg <- esr

  tibble::tibble(
    n = sample_size(x),
    K = nterms(x, include_constant = FALSE),
    nsamples = nsamples(x),
    nchains = nchains(x),
    nsims = nsims(x),
    rhat = rhat_analysis,
    esr = esr_analysis,
    converged = rhat_analysis < rhat_arg & esr_analysis > esr_arg
  )
}
