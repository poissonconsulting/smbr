smb_reanalyse_internal <- function(object, parallel, quiet) {
  timer <- timer::Timer$new()
  timer$start()

  niters <- object$ngens * 2
  nchains <- nchains(object)
  nthin <- niters * nchains / (2000 * 2)

  capture.output(
    stanc <- rstan::stanc(model_code = template(object))
  )
  capture.output(
    stan_model <- rstan::stan_model(
      stanc_ret = stanc, save_dso = TRUE, auto_write = FALSE)
  )

  data <- data_set(object, modify = TRUE, numericize_factors = TRUE)

  monitor <- mbr::monitor(object$model)

  capture.output(
    stan_fit <- rstan::sampling(
      stan_model, data = data, pars = monitor,
      init = object$inits,
      chains = nchains, iter = niters,  warmup = floor(niters/2), thin = nthin,
      cores = ifelse(parallel, nchains, 1L),
      show_messages = !quiet)
  )

  object$mcmcr <- as.mcmcr(stan_fit)
  object$ngens <- as.integer(niters)
  object$duration %<>% magrittr::add(timer$elapsed())
  object
}

smb_reanalyse <- function(object, rhat, nreanalyses,
                          duration, quick, quiet, parallel, glance) {

  if (quick || duration < elapsed(object) * 2 || converged(object, rhat)) {
    if (glance) print(glance(object))
    return(object)
  }

  while (nreanalyses > 0L && duration >= elapsed(object) * 2 && !converged(object, rhat)) {
    object %<>% smb_reanalyse_internal(parallel = parallel, quiet = quiet)
    nreanalyses %<>% magrittr::subtract(1L)
    if (glance) print(glance(object))
  }
  object
}

#' Reanalyse
#'
#' @param object The object to reanalyse.
#' @param rhat A number specifying the rhat threshold.
#' @param nreanalyses A count between 1 and 6 specifying the maximum number of reanalyses.
#' @param duration The maximum total time to spend on analysis and reanalysis.
#' @param quick A flag indicating whether to quickly get unreliable values.
#' @param quiet A flag indicating whether to disable tracing information.
#' @param glance A flag indicating whether to print summary of model.
#' @param beep A flag indicating whether to beep on completion of the analysis.
#' @param parallel A flag indicating whether to perform the analysis in parallel if possible.
#' @param ... Unused arguments.
#' @export
reanalyse.smb_analysis <- function(object,
                                   rhat = getOption("mb.rhat", 1.1),
                                   nreanalyses = getOption("mb.nreanalyses", 1L),
                                   duration = getOption("mb.duration", dhours(1)),
                                   parallel = getOption("mb.parallel", FALSE),
                                   quick = getOption("mb.quick", FALSE),
                                   quiet = getOption("mb.quiet", TRUE),
                                   glance = getOption("mb.glance", TRUE),
                                   beep = getOption("mb.beep", TRUE),
                                   ...) {

  if (beep) on.exit(beepr::beep())

  check_scalar(nreanalyses, c(1L, 6L))
  if (!is.duration(duration)) error("duration must be an object of class Duration")
  check_flag(quick)
  check_flag(quiet)
  check_flag(parallel)
  check_flag(glance)
  check_flag(beep)

  smb_reanalyse(object, rhat = rhat, nreanalyses = nreanalyses,
                duration = duration, quick = quick,
                quiet = quiet, parallel = parallel, glance = glance)
}
