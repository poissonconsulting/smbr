smb_reanalyse_internal <- function(analysis, parallel, quiet) {
  timer <- timer::Timer$new()
  timer$start()

  niters <- analysis$ngens * 2
  nchains <- nchains(analysis)
  nthin <- niters * nchains / (2000 * 2)

  capture.output(
    stanc <- rstan::stanc(model_code = template(analysis))
  )
  capture.output(
    stan_model <- rstan::stan_model(
      stanc_ret = stanc, save_dso = FALSE, auto_write = FALSE)
  )

  capture.output(
    stan_fit <- rstan::sampling(
      stan_model, data = data_set(analysis, modify = TRUE),
      init = analysis$inits,
      chains = nchains, iter = niters,  warmup = floor(niters/2), thin = nthin,
      cores = ifelse(parallel, nchains, 1L),
      show_messages = !quiet)
  )

  analysis$mcmcr <- as.mcmcr(stan_fit)
  analysis$ngens <- as.integer(niters)
  analysis$duration %<>% magrittr::add(timer$elapsed())
  analysis
}

smb_reanalyse <- function(analysis, rhat, duration, quick, quiet, parallel, glance) {

  if (quick || converged(analysis, rhat) || duration < elapsed(analysis) * 2) {
    if (glance) print(glance(analysis))
    return(analysis)
  }

  while (!converged(analysis, rhat) && duration >= elapsed(analysis) * 2) {
    analysis %<>% smb_reanalyse_internal(parallel = parallel, quiet = quiet)
    if (glance) print(glance(analysis))
  }
  analysis
}

#' @export
reanalyse.smb_analysis <- function(analysis,
                                   rhat = getOption("mb.rhat", 1.1),
                                   duration = getOption("mb.duration", dminutes(10)),
                                   parallel = getOption("mb.parallel", FALSE),
                                   quick = getOption("mb.quick", FALSE),
                                   quiet = getOption("mb.quiet", TRUE),
                                   glance = getOption("mb.glance", TRUE),
                                   beep = getOption("mb.beep", TRUE),
                                   ...) {

  if (!is.duration(duration)) error("duration must be an object of class Duration")
  check_flag(quick)
  check_flag(quiet)
  check_flag(parallel)
  check_flag(glance)
  check_flag(beep)

  if (beep) on.exit(beepr::beep())

  smb_reanalyse(analysis, rhat = rhat, duration = duration, quick = quick,
                quiet = quiet, parallel = parallel, glance = glance)
}
