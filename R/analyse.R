

# for testing
# x <- model
#tempfile <- tempfile(fileext = ".stan")
#write(template(x), file = tempfile)


smb_analyse <- function(data, model, quick, quiet, glance, parallel) {

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

  stanfit <- stan(data = data, model_code = template(model),
                      cores = ifelse(parallel, nchains, 1L),
                      init = inits,
                      iter = 2 * niters, thin = nthin, verbose = quiet)

  mcmcr <- extract(stanfit, permute = TRUE) #%>% as.mcmcr()
  #mcmcr <- llply(jags_chains, function(x) x$jags_samples)
  #mcmcr %<>% llply(mcmcr::as.mcmcr)
  #mcmcr %<>% purrr::reduce(mcmcr::bind_chains)

  obj %<>% c(inits = list(inits),
             stan_chains = extract(stanfit, permute = FALSE),
             stanfit = stanfit,
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
                       quick = quick, quiet = quiet, glance = glance,
                       parallel = parallel))
  }

  llply(data, smb_analyse, model = x,
        quick = quick, quiet = quiet, glance = glance, parallel = parallel)
}
