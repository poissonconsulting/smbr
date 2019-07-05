#' @export
pars.smb_code <- function(x, param_type = "all", scalar_only = FALSE, ...) {
  check_vector(param_type, c("fixed", "random", "derived", "primary", "all"), length = 1)
  check_flag(scalar_only)

  if (param_type %in% c("fixed", "random")) {
    error("parameters.smb_code is not currently able to separate 'fixed' or 'random' parameter types - set param_type = 'primary' instead")
  }

  if (param_type == "all") {
    parameters <- c("primary", "derived")

    parameters %<>%
      purrr::map(parameters_arg2to1, x = x, scalar_only = scalar_only) %>%
      unlist() %>%
      sort()

    return(parameters)
  }

  if (param_type == "derived") {
    if (str_detect(x, "transformed parameters\\s*[{]{1}")) {
        parameters <- get_par_names(x, "transformed parameters")
        scalar <- get_par_scalar(x, "transformed parameters")
      } else {
        parameters <- character(0)
        scalar <- logical(0)
      }
  }
  if (param_type == "primary") {
    parameters <- get_par_names(x, "parameters")
    scalar <- get_par_scalar(x, "parameters")
  }

  if (scalar_only) {
    parameters <- parameters[scalar]
  }
  parameters %<>% sort()

  parameters
}

#' @export
pars.smb_model <- function(x, param_type = "all", scalar_only = FALSE, ...) {
  check_vector(param_type, c("fixed", "random", "derived", "primary", "all"),
               length = 1)
  check_flag(scalar_only)

  if (!param_type %in% c("fixed", "random"))
    return(parameters(code(x), param_type = param_type, scalar_only = scalar_only))

  parameters <- parameters(code(x), param_type = "primary", scalar_only = scalar_only)

  random <- names(random_effects(x))
  if (is.null(random)) random <- character(0)
  random %<>%
    intersect(parameters) %>%
    sort()

  if (param_type == "random") return(random)

  parameters %<>%
    setdiff(random) %>%
    sort()

  return(parameters)
}
