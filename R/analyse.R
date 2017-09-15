smb_analyse_chain <- function(inits_chainid, stan_model, data,
                              monitor, seed, ngens, nthin, quiet) {

  capture.output(
    stan_fit <- rstan::sampling(
      stan_model, data = data, init = inits_chainid$inits, pars = monitor,
      seed = seed,
      chains = 1L, iter = ngens, thin = nthin,
      cores = 1L, chain_id = inits_chainid$chain_id,
      show_messages = !quiet)
  )

  stan_fit
}


smb_analyse <- function(data, model, stan_model, quick, quiet, glance, parallel) {

  timer <- timer::Timer$new()
  timer$start()

  nchains <- 4L
  ngens <- model$ngens
  nthin <- ngens * nchains / 4000L

  if (quick) {
    nchains <- 2L
    ngens <- 10L
    nthin <- 1L
  }

  obj <- list(model = model, data = data)

  data %<>% modify_data(model = model, numericize_factors = TRUE)

  inits <- inits(data, model$gen_inits, nchains = nchains)

  inits_chainid <- purrr::imap(inits, function(x, n) {x <- list(inits = x, chain_id = n); x})

  monitor <- mbr::monitor(model)

  # share seed as different chain_ids
  seed <- sample.int(.Machine$integer.max, 1)

  stan_fit <- llply(inits_chainid, .fun = smb_analyse_chain,
                       .parallel = parallel,
                       stan_model = stan_model,
                       data = data,
                       monitor = monitor, seed = seed,
                       ngens = ngens, nthin = nthin,
                       quiet = quiet) %>%
    rstan::sflist2stanfit()

  obj %<>% c(inits = list(inits),
             stanfit = list(stan_fit),
             mcmcr = list(as.mcmcr(stan_fit)),
             ngens = ngens)

  obj$duration <- timer$elapsed()
  class(obj) <- c("smb_analysis", "mb_analysis")

  if (glance) {
    print(glance(obj))
  }
  obj
}

#' @export
analyse.smb_model <- function(x, data,
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

  capture.output(
    stanc <- rstan::stanc(model_code = template(x))
  )
  capture.output(
    stan_model <- rstan::stan_model(
      stanc_ret = stanc, save_dso = FALSE, auto_write = FALSE)
  )

  if (is.data.frame(data)) {
    return(smb_analyse(data = data, model = x, stan_model = stan_model,
                       parallel = parallel,
                       quick = quick, glance = glance, quiet = quiet))
  }

  plyr::llply(data, smb_analyse, model = x, stan_model = stan_model,
              parallel = parallel,
              quick = quick, glance = glance, quiet = quiet)
}
