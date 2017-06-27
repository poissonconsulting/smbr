smb_reanalyse_internal <- function(analysis, parallel, quiet) {

  timer <- timer::Timer$new()
  timer$start()

  niters <- analysis$ngens * 2
  nchains <- dim(as.array(analysis$stan_fit))[2]
  nthin <- niters * nchains / (2000 * 2)

  data <- analysis$data %>% mbr::modify_data(analysis$model)
  stan_fit <- rstan::sampling(analysis$stan_model, data = data,
                              cores = ifelse(parallel, nchains, 1L),
                              init = analysis$inits,
                              iter = 2 * niters, thin = nthin, verbose = quiet,
                              control = analysis$stan_control)

  # Extract posterior
  ex <- rstan::extract(stan_fit, permute = FALSE)

  # Remove lp__ (aka log posterior probability)
  ex <- ex[, , (1:dim(ex)[3])[which(dimnames(ex)$parameters != "lp__")]]

  # List of length equal to number of parameters
  mcmcr <- base::vector(mode = "list", length = dim(ex)[3])
  for (i in 1:length(mcmcr)) {
    cat("parameter", i, "of", length(mcmcr), "\n")
    mcmcr[[i]] <- ex[, , i]
    dim(mcmcr[[i]]) <- c(1, iteration = niters, nchains)
    class(mcmcr[[i]]) <- "mcarray"
  }
  mcmcr %<>% mcmcr::as.mcmcr()

  analysis$stan_fit <- stan_fit
  analysis$mcmcr <- mcmcr
  analysis$ngens <- as.integer(niters)
  analysis$duration %<>% magrittr::add(timer$elapsed())
  analysis
}

smb_reanalyse <- function(analysis, rhat, duration, quick, quiet, parallel) {

  if (quick || converged(analysis, rhat) || duration < elapsed(analysis) * 2) {
    print(glance(analysis))
    return(analysis)
  }

  while (!converged(analysis, rhat) && duration >= elapsed(analysis) * 2) {
    analysis %<>% smb_reanalyse_internal(parallel = parallel, quiet = quiet)
    print(glance(analysis))
  }
  analysis
}


#' @export
reanalyse.smb_analysis <- function(analysis,
                                   rhat = getOption("mb.rhat", 1.1),
                                   duration = getOption("mb.duration",
                                                        lubridate::dminutes(10)),
                                   parallel = getOption("mb.parallel", FALSE),
                                   quick = getOption("mb.quick", FALSE),
                                   quiet = getOption("mb.quiet", TRUE),
                                   beep = getOption("mb.beep", TRUE),
                                   ...) {

  if (!lubridate::is.duration(duration)) error("duration must be an object of class Duration")
  check_flag(quick)
  check_flag(quiet)
  check_flag(parallel)
  check_flag(beep)

  if (beep) on.exit(beepr::beep())

  smb_reanalyse(analysis, rhat = rhat, duration = duration, quick = quick,
                quiet = quiet, parallel = parallel)
}
