smb_reanalyse_internal <- function(analysis, parallel, quiet, ...) {
  timer <- timer::Timer$new()
  timer$start()

  niters <- analysis$ngens * 2
  nchains <- nchains(analysis)
  nthin <- niters * nchains / (2000 * 2)

  # inits should ideally be set from last values...
  stan_fit <- rstan::sampling(analysis$stan_model,
                              data = data_set(analysis, modify = TRUE),
                              chains = nchains, iter = niters * 2L, thin = nthin,
                              init = analysis$inits,
                              cores = ifelse(parallel, nchains, 1L),
                              show_messages = !quiet,
                              ...)

  analysis$stan_fit <- stan_fit
  analysis$mcmcr <- as.mcmcr(stan_fit)
  analysis$ngens <- as.integer(niters)
  analysis$duration %<>% magrittr::add(timer$elapsed())
  analysis
}

# THIS DOESN'T DO ANYTHING AT THE MOMENT BECAUSE I'M NOT SURE IF IT MATTERS, BUT LEAVING IN CASE I WANT TO IMPLEMENT LATER
enough_bfmi <- function(analysis) {

  # code adapted from rstan (https://github.com/stan-dev/rstan/)
  n_e <- 0L
  object <- analysis$stan_fit
  sp <- get_sampler_params(object, inc_warmup = FALSE)
  E <- as.matrix(sapply(sp, FUN = function(x) x[,"energy__"]))
  threshold <- 0.3
  EBFMI <- get_num_upars(object) / apply(E, 2, var)
  n_e <- sum(EBFMI < threshold, na.rm = TRUE)
  # UNCOMMENT THIS TO WORK:  # if (n_e > 0) return(FALSE)

  TRUE

}

smb_reanalyse <- function(analysis, rhat, duration, quick, quiet, parallel, ...) {
  if (quick || (converged(analysis, rhat) && enough_bfmi(analysis)) || duration < elapsed(analysis) * 2) {
    print(glance(analysis))
    return(analysis)
  }

  while (!(converged(analysis, rhat) && enough_bfmi(analysis)) &&
         duration >= elapsed(analysis) * 2) {
    analysis %<>% smb_reanalyse_internal(parallel = parallel, quiet = quiet, ...)
    print(glance(analysis))
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
                                   beep = getOption("mb.beep", TRUE),
                                   ...) {

  if (!is.duration(duration)) error("duration must be an object of class Duration")
  check_flag(quick)
  check_flag(quiet)
  check_flag(parallel)
  check_flag(beep)

  if (beep) on.exit(beepr::beep())

  smb_reanalyse(analysis, rhat = rhat, duration = duration, quick = quick,
                quiet = quiet, parallel = parallel, ...)
}
