#' @export
sd_priors_by.smb_code <- function(
  x, by = 10, distributions = c("normal", "lognormal", "t"), ...) {
  chk_number(by)
  chk_range(by, c(0.001, 1000))
  chk_unused(...)

  chk_s3_class(distributions, "character")
  chk_unique(distributions)
  chk_subset(distributions,  c("laplace", "logistic", "lognormal",
                               "normal", "t"))

  if(!length(distributions)) {
    wrn("No prior distributions included.")
    return(x)
  }

  x <- rm_comments(x)

  pattern2 <- "\\s*[(][^,)]+,\\s*)((\\d+[.]{0,1}\\d*)|(\\d*[.]{0,1}\\d+))([)])"
  pattern3 <- "\\s*[(][^,)]+,\\s*)((\\d+[.]{0,1}\\d*)|(\\d*[.]{0,1}\\d+))(,[^,)]+[)])"
  replacement <- paste0("\\1\\2 * ", by, ")\\6")
  if("laplace" %in% distributions)
    x <- gsub(paste0("(~\\s*double_exponential", pattern2), replacement, x)
  if("logistic" %in% distributions)
    x <- gsub(paste0("(~\\s*logistic", pattern2), replacement, x)
  if("lognormal" %in% distributions)
    x <- gsub(paste0("(~\\s*lognormal", pattern2), replacement, x)
  if("normal" %in% distributions)
    x <- gsub(paste0("(~\\s*normal", pattern2), replacement, x)
  if("t" %in% distributions)
    x <- gsub(paste0("(~\\s*student_t", pattern3), replacement, x)

  mb_code(x)
}
