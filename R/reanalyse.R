#' @export
reanalyse1.smb_analysis <- function(object, parallel, quiet, ...) {
  timer <- timer::Timer$new()
  timer$start()

  niters <- niters(object)
  nthin <- nthin(object) * 2L

  loaded <- load_model(object, quiet)

  data <- data_set(object, modify = TRUE, numericize_factors = TRUE)

  inits_chainid <- purrr::imap(object$inits, function(x, n) {x <- list(inits = x, chain_id = n); x})

  monitor <- mbr::monitor(object$model)

  # share seed as different chain_ids
  seed <- sample.int(.Machine$integer.max, 1)

  stan_fit <- llply(inits_chainid, .fun = smb_analyse_chain,
                    .parallel = parallel,
                    loaded = loaded,
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
