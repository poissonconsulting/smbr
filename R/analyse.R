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

  # New code saves template and sets directory for Stan compilation so it can be used again with reanalyse without recompiling
  subfoldr::save_template(model$code %>% as.character(), "stan_model")
  file <- stringr::str_c(subfoldr::get_main(), "templates",
                         subfoldr::get_sub(),
                         "stan_model.txt", sep = "/")
  s <- readr::read_file(file) %>% str_c("\n") # complete final line
  readr::write_file(s, file)
  rm(s)
  base::file.copy(file, stringr::str_replace(file, "txt$", "stan"),
                  overwrite = TRUE)
  file %<>% stringr::str_replace("txt$", "stan")
  stan_model <- rstan::stan_model(file = file, auto_write = TRUE)

  # OLD: stan_model <- rstan::stan_model(model_code = model$code %>% as.character())
  stan_model <- rstan::stan_model(file = file, auto_write = TRUE)
  stan_fit <- rstan::sampling(stan_model, data = data,
                      cores = ifelse(parallel, nchains, 1L),
                      init = inits,
                      iter = 2 * niters, thin = nthin, verbose = quiet,
                      control = stan_control)

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

  obj %<>% c(inits = list(inits),
             stan_fit = stan_fit,
             stan_model = stan_model,
             stan_control = stan_control,
             mcmcr = list(mcmcr), ngens = niters)
  obj$duration <- timer$elapsed()
  class(obj) <- c("smb_analysis", "mb_analysis")

  if (glance) print(glance(obj))

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
