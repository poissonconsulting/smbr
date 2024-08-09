#' @export
pars.smb_code <- function(x, param_type = "all", scalar = NULL, ...) {
  chk_string(param_type)
  chk_subset(param_type, c("fixed", "random", "derived", "primary", "all"))
  if (!is.null(scalar)) chk_flag(scalar)
  chk_unused(...)

  if (param_type %in% c("fixed", "random")) {
    error("pars.smb_code is not currently able to separate 'fixed' or 'random' parameter types - set param_type = 'primary' instead")
  }

  if (param_type == "all") {
    pars <- c("primary", "derived")

    pars %<>%
      purrr::map(pars_arg2to1, x = x, scalar = scalar) %>%
      unlist() %>%
      sort()

    return(pars)
  }

  if (param_type == "derived") {
    if (str_detect(template(x), "transformed parameters\\s*[{]{1}")) {
      pars <- get_par_names(x, "transformed parameters")
      scalars <- get_par_scalar(x, "transformed parameters")
    } else {
      pars <- character(0)
      scalars <- logical(0)
    }
  }
  if (param_type == "primary") {
    pars <- get_par_names(x, "parameters")
    scalars <- get_par_scalar(x, "parameters")
  }
  if (isTRUE(scalar)) {
    pars <- pars[scalars]
  } else if (isFALSE(scalar)) {
    pars <- pars[!scalars]
  }

  pars %<>% sort()

  pars
}

#' @export
pars.smb_model <- function(x, param_type = "all", scalar = NULL, ...) {
  chk_string(param_type)
  chk_subset(param_type, c("fixed", "random", "derived", "primary", "all"))
  chk_null_or(scalar, vld = vld_flag)
  chk_unused(...)

  if (param_type == "derived") {
    return(sort(x$derived))
  }

  if (param_type == "all") {
    pars <- c("primary", "derived")

    pars %<>%
      purrr::map(pars_arg2to1, x = x, scalar = scalar) %>%
      unlist() %>%
      sort()

    return(pars)
  }

  if (!param_type %in% c("fixed", "random")) {
    return(pars(code(x), param_type = param_type, scalar = scalar))
  }

  pars <- pars(code(x), param_type = "primary", scalar = scalar)

  random <- names(random_effects(x))
  if (is.null(random)) random <- character(0)
  random %<>%
    intersect(pars) %>%
    sort()

  if (param_type == "random") {
    return(random)
  }

  pars %<>%
    setdiff(random) %>%
    sort()

  return(pars)
}
