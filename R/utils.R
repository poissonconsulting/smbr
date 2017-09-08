# should be more stable than log(colMeans(exp(x)))
logColMeansExp <- function(x) {
  matrixStats::colLogSumExps(x) - log(nrow(x))
}

parameters_arg2to1 <- function(param_type, x, scalar_only) {
  parameters(x = x, param_type = param_type, scalar_only = scalar_only)
}

clean_blocks <- function(x) {

  # Remove linebreaks and extra white space
  x %<>% str_replace_all(";.*\n", ";\n") %>%
    str_replace_all("\n", "") %>%
    str_replace_all("\\s{2,}", " ")
  x
}

get_block_location <- function(x, block_name) {

  if(!str_detect(x, block_name)) return(character(0))
  if(length(x) > 1) {
    x <- x[1]
    warning("length(x) > 1; using first element")
  }

  possible_blocks <- c("data", "transformed data", "parameters",
                       "transformed parameters", "model", "generated quantities")

  block_names <- str_extract_all(x, "(^|[}])[^(}|{)]+[{]", simplify = TRUE) %>%
    str_replace_all("([{]|[}])", "") %>%
    str_replace_all("(^\\s+|\\s+$)", ""); block_names

  if(!block_name %in% block_names) return(character(0))

  block_locs <- str_locate_all(x, "(^|[}])[^(}|{)]+[{]")[[1]] %>% t()

  if(ncol(block_locs) != length(block_names)) stop("Number of Stan blocks does not match number of block names")

  n_block <- ncol(block_locs)
  block_locs[1:(2 * n_block - 1)] <- block_locs %>% magrittr::extract(2:(2 * n_block))
  block_locs[2 * n_block] <- nchar(x)
  block_locs %<>% t() %>% set_rownames(block_names)

  which_block <- which(block_names == block_name)

  block_locs[which_block, ] %>% as.list()

}

extract_pars <- function(x, block_location) {

  x %<>% str_sub(block_location$start, block_location$end) %>%
    str_trim() %>%
    str_replace_all("([{]|[}])", "") %>%
    str_trim() %>%
    str_replace(";$", "") %>%
    str_split(";", simplify = TRUE) %>%
    str_trim(); x

  type <- c("int", "real", "vector", "simplex", "ordered", "row_vector",
            "matrix", "corr_matrix", "cov_matrix", "positive_ordered") %>%
    str_c(collapse = "|") %>%
    str_c("(", ., ")")

#  x <- "vector<lower=0>[N] name"
  pattern <- str_c(type, "[<[^>]+>]*?\\s+(\\w+)$")
  pars <- str_replace(x[str_detect(x, pattern)], pattern, "\\2")

  pars

}

get_par_names <- function(x, block_name) {
  x %<>% rm_comments() %>% clean_blocks()
  block_location <- get_block_location(x, block_name)
  extract_pars(x, block_location)
}

extract_types <- function(x, block_location) {

  x %<>% str_sub(block_location$start, block_location$end) %>%
    str_trim() %>%
    str_replace_all("([{]|[}])", "") %>%
    str_trim() %>%
    str_replace(";$", "") %>%
    str_split(";", simplify = TRUE) %>%
    str_trim(); x

  type <- c("int", "real", "vector", "simplex", "ordered", "row_vector",
            "matrix", "corr_matrix", "cov_matrix", "positive_ordered") %>%
    str_c(collapse = "|") %>%
    str_c("(", ., ")")

  pattern <- str_c(type, "[<[^>]+>]*?\\s+(\\w+)$")
  x %<>% magrittr::extract(str_detect(x, pattern))

  types <- str_extract(x, type)

  types

}

get_par_types <- function(x, block_name) {
  x %<>% rm_comments() %>% clean_blocks()
  block_location <- get_block_location(x, block_name)
  extract_types(x, block_location)
}
