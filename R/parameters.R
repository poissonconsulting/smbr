#' @export
parameters.smb_code <- function(x, param_type = "all", scalar_only = FALSE, ...) {
  check_scalar(param_type, c("fixed", "random", "derived", "primary", "all"))
  check_flag(scalar_only)

  if (param_type %in% c("fixed", "random"))
    error("parameters.smb_code is not currently able to separate 'fixed' or 'random' parameter types - set param_type = 'primary' instead")

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
      types <- get_par_types(x, "transformed parameters")
    } else {
      parameters <- character(0)
      types <- character(0)
    }
  if (param_type == "primary") {
    parameters <- get_par_names(x, "parameters")
    types <- get_par_types(x, "parameters")
  }

  if (scalar_only) parameters <- parameters[types %in% c("int", "real")]

  parameters %<>% sort()

  parameters
}


#' @export
parameters.smb_model <- function(x, param_type = "all", scalar_only = FALSE, ...) {
  check_scalar(param_type, c("fixed", "random", "derived", "primary", "all"))
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

#' @export
parameters.smb_analysis <- function(x, param_type = "all", scalar_only = FALSE, ...) {

  check_scalar(param_type, c("fixed", "random", "derived", "primary", "all"))
  check_flag(scalar_only)

  if (param_type %in% c("primary", "all")) {
    parameters <- c("fixed", "random")
    if (param_type == "all") parameters %<>% c("derived")

    parameters %<>%
      purrr::map(parameters_arg2to1, x = x, scalar_only = scalar_only) %>%
      unlist() %>%
      sort()

    return(parameters)
  }

  parameters <- parameters(as.mcmcr(x), scalar_only = scalar_only)

  random <- names(random_effects(x))
  if (is.null(random)) random <- character(0)
  random %<>%
    intersect(parameters) %>%
    sort()

  if (param_type ==  "random") return(random)

  derived <- x$model$derived %>%
    intersect(parameters) %>%
    sort()

  if (param_type == "derived") return(derived)

  # Exclude parameters only in generated quanities from mcmcr
  if (str_detect(code(x$model), "generated quantities\\s*[{]{1}")) {
    gq_par <- get_par_names(code(x$model), "generated quantities") %>%
      .[!. %in% parameters(x$model)]
  } else {
    gq_par <- character(0)
  }

  parameters %<>%
    setdiff(gq_par) %>%
    setdiff(random) %>%
    setdiff(derived) %>%
    sort()

  parameters

}
