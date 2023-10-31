#' @export
check_model_pars.smb_code <- function(x, fixed, random, derived, drops) {
  chk_string(fixed)
  chk_null_or(random, vld = vld_character)
  chk_null_or(derived, vld = vld_character)
  chk_null_or(drops, vld = vld_character)

  if (!any(str_detect(pars(x, param_type = "primary"), fixed))) {
    error("fixed does not match any primary code parameters")
  }

  if (length(random) && !all(random %in% pars(x, param_type = "primary"))) {
    error("random effects parameters missing from primary code parameters")
  }

  if (length(derived) && !all(derived %in% pars(x, param_type = "derived"))) {
    error("derived parameters missing from derived code parameters")
  }

  if (length(drops) && !all(unlist(drops) %in% pars(x, param_type = "primary", scalar = TRUE))) {
    error("drops parameters missing from primary scalar code parameters")
  }
  derived_pars <- pars(x, param_type = "derived")
  invisible(intersect(derived, derived_pars))
}
