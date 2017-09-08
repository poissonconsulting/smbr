#' @export
check_model_parameters.smb_code <- function(x, fixed, random, derived, drops) {
  check_string(fixed)
  checkor(check_null(random), check_vector(random, character(0), min_length = 0))
  checkor(check_null(derived), check_vector(derived, character(0), min_length = 0))
  checkor(check_null(drops), check_vector(drops, character(0), min_length = 0))

  if (!any(str_detect(parameters(x, param_type = "primary"), fixed)))
    error("fixed does not match any primary code parameters")

  if (length(random) && !all(random %in% parameters(x, param_type = "primary")))
    error("random effects parameters missing from primary code parameters")

  if (length(derived) && !all(derived %in% parameters(x, param_type = "derived")))
    error("derived parameters missing from derived code parameters")

  if (length(drops) && !all(unlist(drops) %in% parameters(x, param_type = "primary", scalar_only = TRUE)))
    error("drops parameters missing from primary scalar code parameters")

  parameters(x, param_type = "derived")
}
