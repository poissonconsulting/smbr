drop_parameter <- function(x, parameter) {
  if (!parameter %in% parameters(x, "primary", scalar_only = TRUE))
    error("parameter '", parameter, "' is not an (untransformed) scalar (int or real) parameter in code")

  parameters <- get_par_names(x)
  if (length(parameters) == 1) error("attempting to drop last parameter!")

  type <- get_par_types(x)[get_par_names(x) == parameter]

  pattern <- str_c(type, "\\s*(<[^>]+>){0,1}\\s*", parameter, "\\s*;")
  x$template %<>% str_replace(pattern, "")

  x$template %<>% paste_transformed_data(str_c(type, " ", parameter, ";\n"))
  x$template %<>% paste_transformed_data(str_c(parameter, " = 0;"), top = FALSE)
  x
}

#' @export
drop_parameters.smb_code <- function(x, parameters = character(0), ...) {
  check_vector(parameters, "")
  check_unique(parameters)

  if (!length(parameters)) return(x)

  for (parameter in parameters) x %<>% drop_parameter(parameter)

  x
}
