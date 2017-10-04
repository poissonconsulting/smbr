smb_reanalyse_internal <- function(object, parallel, quiet) {
  timer <- timer::Timer$new()
  timer$start()

  niters <- niters(object)
  nthin <- nthin(object) * 2L

  stan_model <- load_model(object, quiet)

  data <- data_set(object, modify = TRUE, numericize_factors = TRUE)

  inits_chainid <- purrr::imap(object$inits, function(x, n) {x <- list(inits = x, chain_id = n); x})

  monitor <- mbr::monitor(object$model)

  # share seed as different chain_ids
  seed <- sample.int(.Machine$integer.max, 1)

  stan_fit <- llply(inits_chainid, .fun = smb_analyse_chain,
                    .parallel = parallel,
                    stan_model = stan_model,
                    data = data,
                    monitor = monitor, seed = seed,
                    niters = niters, nthin = nthin,
                    quiet = quiet) %>%
    rstan::sflist2stanfit()

  object$stan_fit <- stan_fit
  object$mcmcr <- as.mcmcr(stan_fit)
  object$nthin <- as.integer(nthin)
  object$duration <- timer$elapsed()
  object
}

smb_reanalyse <- function(object, rhat, esr, nreanalyses,
                          duration, quiet, parallel, glance) {

  if (duration < elapsed(object) * 2 || (converged(object, rhat) && esr(object) > esr)) {
    if (glance) print(glance(object))
    return(object)
  }

  while (nreanalyses > 0L && duration >= elapsed(object) * 2 && !(converged(object, rhat) && esr(object) > esr)) {
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
#' @param esr A number specifying the minimum effective sampling rate.
#' @param nreanalyses A count between 1 and 6 specifying the maximum number of reanalyses.
#' @param duration The maximum total time to spend on analysis and reanalysis.
#' @param quiet A flag indicating whether to disable tracing information.
#' @param glance A flag indicating whether to print summary of model.
#' @param beep A flag indicating whether to beep on completion of the analysis.
#' @param parallel A flag indicating whether to perform the analysis in parallel if possible.
#' @param ... Unused arguments.
#' @export
reanalyse.smb_analysis <- function(object,
                                   rhat = getOption("mb.rhat", 1.1),
                                   esr = getOption("mb.esr", 0.33),
                                   nreanalyses = getOption("mb.nreanalyses", 1L),
                                   duration = getOption("mb.duration", dhours(1)),
                                   parallel = getOption("mb.parallel", FALSE),
                                   quiet = getOption("mb.quiet", TRUE),
                                   glance = getOption("mb.glance", TRUE),
                                   beep = getOption("mb.beep", TRUE),
                                   ...) {

  check_flag(beep)
  if (beep) on.exit(beepr::beep())

  check_scalar(nreanalyses, c(1L, 6L))
  if (!is.duration(duration)) error("duration must be an object of class Duration")
  check_flag(quiet)
  check_flag(parallel)
  check_flag(glance)
  check_number(esr, c(0, 1))

  smb_reanalyse(object, rhat = rhat, esr = esr, nreanalyses = nreanalyses,
                duration = duration,
                quiet = quiet, parallel = parallel, glance = glance)
}
