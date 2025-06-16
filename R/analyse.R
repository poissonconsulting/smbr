smb_analyse_chain <- function(inits_chainid, loaded, data,
                              monitor, seed, niters, nthin, quiet,
                              niters_warmup) {
  capture_output <- if (quiet) function(x) suppressWarnings(capture.output(x)) else identity

  inits <- inits_chainid$inits
  if (is.list(inits)) inits <- list(inits)

  iter <- niters * nthin + niters_warmup

  capture_output(
    stan_fit <- rstan::sampling(
      loaded,
      data = data, init = inits, pars = monitor,
      warmup = niters_warmup,
      seed = seed,
      chains = 1L, iter = iter, thin = nthin,
      cores = 1L, chain_id = inits_chainid$chain_id,
      show_messages = !quiet
    )
  )

  stan_fit
}


#' @export
analyse1.smb_model <- function(model, data, loaded, nchains, niters, nthin,
                               quiet, glance, parallel, seed, niters_warmup, ...) {
  timer <- timer::Timer$new()
  timer$start()

  obj <- list(model = model, data = data)

  data %<>% modify_data(model = model, numericize_factors = TRUE)

  inits <- inits(data, model$gen_inits, nchains = nchains)
  inits_chainid <- purrr::imap(inits, function(x, n) {
    x <- list(inits = x, chain_id = n)
    x
  })

  monitor <- embr::monitor(model)

  stan_fit <- llply(inits_chainid,
    .fun = smb_analyse_chain,
    .parallel = parallel,
    loaded = loaded,
    data = data,
    monitor = monitor, seed = seed,
    niters = niters, nthin = nthin,
    quiet = quiet, niters_warmup = niters_warmup
  ) %>%
    rstan::sflist2stanfit()

  obj %<>% c(
    inits = list(inits),
    stanfit = list(stan_fit),
    mcmcr = list(as.mcmcr(stan_fit)),
    nthin = nthin
  )

  obj$duration <- timer$elapsed()
  class(obj) <- c("smb_analysis", "mb_analysis")

  if (glance) print(glance(obj))

  obj
}
