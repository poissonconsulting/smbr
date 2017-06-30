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
                              iter = 2 * niters, thin = nthin,
                              verbose = quiet,
                              control = analysis$stan_control)

  # Extract posterior
  ex <- rstan::extract(stan_fit, permute = FALSE)

  # Remove lp__ (aka log posterior probability)
  ex <- ex[, , (1:dim(ex)[3])[which(dimnames(ex)$parameters != "lp__")]]

  # List of length equal to number of parameters
  mcmcr <- base::vector(mode = "list", length = dim(ex)[3])
  for (i in 1:length(mcmcr)) {
    mcmcr[[i]] <- ex[, , i]
    dim(mcmcr[[i]]) <- c(1, iteration = niters / nthin, nchains)
    class(mcmcr[[i]]) <- "mcarray"
  }
  mcmcr %<>% mcmcr::as.mcmcr()
  names(mcmcr) <- attr(ex, "dimnames")$parameters

  analysis$stan_fit <- stan_fit
  analysis$mcmcr <- mcmcr
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

smb_reanalyse <- function(analysis, rhat, duration, quick, quiet, parallel) {

  if (quick || duration < elapsed(analysis) * 2) {
    print(glance(analysis))
    return(analysis)
  }

  cnvrgd <- converged(analysis, rhat)

  if (cnvrgd & enough_bfmi(analysis)) {
    print(glance(analysis))
    return(analysis)
  }

  while ((!cnvrgd | !enough_bfmi(analysis)) &&
         duration >= elapsed(analysis) * 2) {
    analysis %<>% smb_reanalyse_internal(parallel = parallel, quiet = quiet)
    analysis$stan_warnings <- warnings()
    cnvrgd <- converged(analysis, rhat)
    print(glance(analysis))
  }

  analysis

}

#' @export

reanalyse.smb_analysis <- function(analysis,
                                   rhat = getOption("mb.rhat", 1.1),
                                   duration = getOption("mb.duration",
                                                        dminutes(10)),
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
                quiet = quiet, parallel = parallel)
}
