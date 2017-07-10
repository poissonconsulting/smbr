smb_analyse <- function(data, model, quick, quiet, glance, parallel, ...) {

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

  data %<>% modify_data(model = model)

  inits <- model$gen_inits(data)

  if (identical(inits, list())) inits <- "random"

  regexp <- model$fixed
  named <- names(model$random_effects) %>% c(model$derived)

  if (quiet) {
    suppressWarnings(stan_model <- rstan::stan_model(model_code = template(model), verbose = FALSE, auto_write = TRUE))
  } else
    stan_model <- rstan::stan_model(model_code = template(model), verbose = TRUE, auto_write = TRUE)

  # use named and regexp and parameters to set pars
  stan_fit <- rstan::sampling(stan_model, data = data,
                              chains = nchains, iter = niters * 2L, thin = nthin,
                              init = inits, cores = ifelse(parallel, nchains, 1L),
                              show_messages = FALSE,
                              ...)

  obj %<>% c(inits = list(inits),
             stan_model = stan_model,
             stan_fit = stan_fit,
             mcmcr = list(as.mcmcr(stan_fit)),
             ngens = niters)

  obj$duration <- timer$elapsed()
  class(obj) <- c("smb_analysis", "mb_analysis")

  if (glance) print(glance(obj))

  obj
}

#' @export
#' @param ... Additional arguments passed to rstan::sampling
analyse.smb_model <- function(x, data,
                              parallel = getOption("mb.parallel", FALSE),
                              quick = getOption("mb.quick", FALSE),
                              quiet = getOption("mb.quiet", TRUE),
                              glance = getOption("mb.glance", TRUE),
                              beep = getOption("mb.beep", TRUE),
                              ...) {
  if (is.data.frame(data)) {
    check_data2(data)
  } else if (is.list(data)) {
    llply(data, check_data2)
  } else error("data must be a data.frame or a list of data.frames")

  check_flag(quick)
  check_flag(quiet)
  check_flag(parallel)
  check_flag(glance)
  check_flag(beep)

  if (beep) on.exit(beepr::beep())

  if (is.data.frame(data)) {
    return(smb_analyse(data = data, model = x,
                       quick = quick, quiet = quiet,
                       glance = glance, parallel = parallel, ...))
  }

  llply(data, smb_analyse, model = x, quick = quick, quiet = quiet,
        glance = glance, parallel = parallel, ...)
}
