smb_analyse_chain <- function(inits_chainid, stan_model, data,
                              monitor, seed, niters, nthin, quiet) {

  capture_output <- if (quiet) function(x) suppressWarnings(capture.output(x)) else eval

  capture_output(
    stan_fit <- rstan::sampling(
      stan_model, data = data, init = inits_chainid$inits, pars = monitor,
      seed = seed,
      chains = 1L, iter = niters * 2L * nthin, thin = nthin,
      cores = 1L, chain_id = inits_chainid$chain_id,
      show_messages = !quiet)
  )

  stan_fit
}


smb_analyse <- function(data, model, stan_model, nchains, niters, nthin, quiet, glance, parallel) {
  timer <- timer::Timer$new()
  timer$start()

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
                    niters = niters, nthin = nthin,
                    quiet = quiet) %>%
    rstan::sflist2stanfit()

  obj %<>% c(inits = list(inits),
             stanfit = list(stan_fit),
             mcmcr = list(as.mcmcr(stan_fit)),
             nthin = nthin)

  obj$duration <- timer$elapsed()
  class(obj) <- c("smb_analysis", "mb_analysis")

  if (glance) print(glance(obj))

  obj
}

#' @export
analyse.smb_model <- function(x, data,
                              nchains = getOption("mb.nchains", 3L),
                              niters = getOption("mb.niters", 1000L),
                              nthin = getOption("mb.thin", NULL),
                              parallel = getOption("mb.parallel", FALSE),
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

  check_count(nchains, c(2L, 10L))
  check_count(niters, c(10L, 100000L))
  checkor(check_null(nthin), check_count(nthin, c(1L, 10000L)))

  check_flag(parallel)
  check_flag(quiet)
  check_flag(glance)

  if (is.null(nthin)) nthin <- nthin(x)

  capture_output <- if (quiet) function(x) suppressWarnings(capture.output(x)) else eval

  capture_output(
    stanc <- rstan::stanc(model_code = template(x))
  )
  capture_output(
    stan_model <- rstan::stan_model(
      stanc_ret = stanc, save_dso = FALSE, auto_write = FALSE)
  )

  if (is.data.frame(data)) {
    return(smb_analyse(data = data, model = x, stan_model = stan_model,
                       nchains = nchains, niters = niters, nthin = nthin,
                       parallel = parallel, quiet = quiet, glance = glance))
  }

  plyr::llply(data, smb_analyse, model = x, stan_model = stan_model,
              nchains = nchains, niters = niters, nthin = nthin,
              parallel = parallel, quiet = quiet, glance = glance)
}
