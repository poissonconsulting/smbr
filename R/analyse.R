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

  # Check that terms aren't used in both new_expr and generated quanities block


  if (is.data.frame(data)) {
    return(smb_analyse(data = data, model = x,
                       quick = quick, quiet = quiet,
                       glance = glance, parallel = parallel, ...))
  }

  llply(data, smb_analyse, model = x, quick = quick, quiet = quiet,
        glance = glance, parallel = parallel, ...)
}

#' Analyse
#'
#' @param x An object inheriting from class smb_models.
#' @param data The data frame to analyse.
#' @param parallel A flag indicating whether to perform the analysis in parallel if possible.
#' @param quick A flag indicating whether to quickly get unreliable values.
#' @param quiet A flag indicating whether to disable tracing information.
#' @param glance A flag indicating whether to print a model summary.
#' @param beep A flag indicating whether to beep on completion of the analysis.
#' @param ...  Additional arguments.
#' @export
analyse.smb_models <- function(x, data,
                               parallel = getOption("mb.parallel", FALSE),
                               quick = getOption("mb.quick", FALSE),
                               quiet = getOption("mb.quiet", TRUE),
                               glance = getOption("mb.glance", TRUE),
                               beep = getOption("mb.beep", TRUE),
                               ...) {

  check_flag(beep)
  if (beep) on.exit(beepr::beep())

  names <- names(x)
  if (is.null(names)) names(x) <- 1:length(x)

  analyses <- purrr::map(x, analyse, data = data, parallel = parallel,
                         quick = quick, quiet = quiet, glance = glance,
                         beep = FALSE, ...)

  as_smb_analyses <- function(x, names) {
    names(x) <- names
    class(x) <- "smb_analyses"
    x
  }

  if (is.data.frame(data)) {
    analyses %<>% as_smb_analyses(names = names)
    return(analyses)
  }
  analyses %<>% purrr::transpose()
  analyses %<>% purrr::map(as_smb_analyses, names = names)
  analyses

}
