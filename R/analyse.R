smb_analyse <- function(data, model, stan_model, seed, quick, quiet, glance, parallel) {

  timer <- timer::Timer$new()
  timer$start()

  nchains <- 4L
  niters <- model$niters
  nthin <- niters * nchains / (2000 * 2)

  if (quick) {
    nchains <- 2L
    niters <- 10
    nthin <- 1L
  }

  obj <- list(model = model, data = data)

  data %<>% modify_data(model = model, numericize_factors = TRUE)

  inits <- inits(data, model$gen_inits, nchains = nchains)

  regexp <- model$fixed
  named <- names(model$random_effects) %>%
    c(model$derived)

  capture.output(
    stan_fit <- rstan::sampling(
      stan_model, data = data, init = inits,
      chains = nchains, iter = niters, warmup = floor(niters/2), thin = nthin,
      cores = ifelse(parallel, nchains, 1L), seed = seed,
      show_messages = !quiet)
  )

  obj %<>% c(inits = list(inits),
             mcmcr = list(as.mcmcr(stan_fit)),
             ngens = niters,
             seed = seed)

  obj$duration <- timer$elapsed()
  class(obj) <- c("smb_analysis", "mb_analysis")

  if (glance) {
    print(glance(obj))
  }
  obj
}

#' @export
analyse.smb_model <- function(x, data, seed = NA,
                              parallel = getOption("mb.parallel", FALSE),
                              quick = getOption("mb.quick", FALSE),
                              quiet = getOption("mb.quiet", TRUE),
                              glance = getOption("mb.glance", TRUE),
                              beep = getOption("mb.beep", TRUE),
                              ...) {

  check_flag(beep)
  if (beep) on.exit(beepr::beep())

  if (is.data.frame(data)) {
    check_data2(data)
  } else if (is.list(data)) {
    llply(data, check_data2)
  } else error("data must be a data.frame or a list of data.frames")

  check_flag(quick)
  check_flag(quiet)
  check_flag(parallel)
  check_flag(glance)

  seed %<>% as.integer()
  if (identical(seed, NA_integer_))
    seed <- sample.int(.Machine$integer.max, 1)
  check_count(seed)

  capture.output(
    stanc <- rstan::stanc(model_code = template(x))
  )
  capture.output(
    stan_model <- rstan::stan_model(
      stanc_ret = stanc, save_dso = FALSE, auto_write = FALSE)
  )

  if (is.data.frame(data)) {
    return(smb_analyse(data = data, model = x, stan_model = stan_model,
                       seed = seed, parallel = parallel,
                       quick = quick, glance = glance, quiet = quiet))
  }

  plyr::llply(data, smb_analyse, model = x, stan_model = stan_model,
              seed = seed, parallel = parallel,
              quick = quick, glance = glance, quiet = quiet)
}
