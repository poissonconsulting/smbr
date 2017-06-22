smb_analyse <- function(data, model, quick, quiet, glance, parallel,
                        stan_control) {

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

  inits <- if(identical(model$gen_inits(data), list())) {
    "random"
  } else {
    model$gen_inits(data)
  }

  regexp <- model$fixed
  named <- names(model$random_effects) %>% c(model$derived)

  # nchains <- 1
  stan_model <- rstan::stan_model(model_code = model$code %>% as.character())
  stan_fit <- rstan::sampling(stan_model, data = data,
                      cores = ifelse(parallel, nchains, 1L),
                      init = inits,
                      iter = 2 * niters, thin = nthin, verbose = quiet,
                      control = stan_control)
  cat("Finished stan.\n")

  # Extract posterior
  ex <- rstan::extract(stan_fit, permute = FALSE)

  # Remove lp__ (aka log posterior probability)
  ex <- ex[, , (1:dim(ex)[3])[which(dimnames(ex)$parameters != "lp__")]]

  # List of length equal to number of parameters
  mcmcr <- base::vector(mode = "list", length = dim(ex)[3])
  for (i in 1:length(mcmcr)) {
    cat("parameter", i, "of", length(mcmcr), "\n")
    mcmcr[[i]] <- ex[, , i]
    dim(mcmcr[[i]]) <- c(1, iteration = 1000, 4)
    class(mcmcr[[i]]) <- "mcarray"
  }
  mcmcr %<>% mcmcr::as.mcmcr()
  #mcmcr %<>% purrr::reduce(mcmcr::bind_chains)
  cat("Converted to MCMCR.\n")

  obj %<>% c(inits = list(inits),
             stan_fit = stan_fit,
             mcmcr = list(mcmcr), ngens = niters)
  obj$duration <- timer$elapsed()
  class(obj) <- c("smb_analysis", "mb_analysis")
  cat("Made obj.\n")

  if (glance) print(glance(obj))
  cat("Glanced.\n")

  obj
}

#' @export
analyse.smb_model <- function(x, data,
                              parallel = getOption("mb.parallel", FALSE),
                              quick = getOption("mb.quick", FALSE),
                              quiet = getOption("mb.quiet", TRUE),
                              glance = getOption("mb.glance", TRUE),
                              beep = getOption("mb.beep", TRUE),
                              stan_control = NULL,
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
    return(smb_analyse(data = data, model = x, quick = quick, quiet = quiet,
                       glance = glance, parallel = parallel,
                       stan_control = stan_control))
  }

  llply(data, smb_analyse, model = x, quick = quick, quiet = quiet,
        glance = glance, parallel = parallel, stan_control = stan_control)

}
