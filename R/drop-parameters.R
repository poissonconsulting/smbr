drop_parameter <- function(x, parameter) {
  if (!parameter %in% pars(x, "primary", scalar = TRUE))
    error("parameter '", parameter, "' is not an (untransformed) scalar (int or real) parameter in code")

  pars <- get_par_names(x)
  if (length(pars) == 1) error("attempting to drop last parameter!")

  type <- get_par_types(x)[get_par_names(x) == parameter]

  pattern <- str_c(type, "\\s*(<[^>]+>){0,1}\\s*", parameter, "\\s*;")
  x$template %<>% str_replace(pattern, "")

  x$template %<>% paste_transformed_data(str_c(type, " ", parameter, ";\n"))
  x$template %<>% paste_transformed_data(str_c(parameter, " = 0;"), top = FALSE)
  x
}

#' @export
drop_pars.smb_code <- function(x, pars = character(0), ...) {
  chk_character(pars)
  chk_unique(pars)

  if (!length(pars)) return(x)

  for (parameter in pars) x %<>% drop_parameter(parameter)

  x
}
