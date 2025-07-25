clean_blocks <- function(x) {
  x %<>% template() %>%
    str_replace_all(";.*\n", ";\n") %>%
    str_replace_all("\n", "") %>%
    str_replace_all("\\s{2,}", " ")
  x
}

get_block_location <- function(x, block_name) {
  if (!str_detect(x, block_name)) {
    return(character(0))
  }
  if (length(x) > 1) {
    x <- x[1]
    warning("length(x) > 1; using first element")
  }

  block_names <- str_extract_all(x, "(^|[}])[^(}|{)]+[{]", simplify = TRUE) %>%
    str_replace_all("([{]|[}])", "") %>%
    str_replace_all("(^\\s+|\\s+$)", "")
  block_names

  if (!block_name %in% block_names) {
    return(character(0))
  }

  block_locs <- str_locate_all(x, "(^|[}])[^(}|{)]+[{]")[[1]] %>% t()

  if (ncol(block_locs) != length(block_names)) stop("Number of Stan blocks does not match number of block names")

  n_block <- ncol(block_locs)
  block_locs[1:(2 * n_block - 1)] <- block_locs %>%
    magrittr::extract(2:(2 * n_block))
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
    str_trim()
  x

  type <- c(
    "int", "real", "vector", "simplex", "ordered", "row_vector",
    "matrix", "corr_matrix", "cov_matrix", "positive_ordered", "array"
  ) %>%
    str_c(collapse = "|") %>%
    str_c("(", ., ")")

  pattern <- str_c(type, "[<[^>]+>]*\\s+(\\w+)(\\[\\w+(,\\w+)*\\])?$")
  pars <- str_replace(x[str_detect(x, pattern)], pattern, "\\2")

  pars
}

extract_scalar <- function(x, block_location) {
  x %<>% str_sub(block_location$start, block_location$end) %>%
    str_trim() %>%
    str_replace_all("([{]|[}])", "") %>%
    str_trim() %>%
    str_replace(";$", "") %>%
    str_split(";", simplify = TRUE) %>%
    str_trim()
  x

  type <- c(
    "int", "real", "vector", "simplex", "ordered", "row_vector",
    "matrix", "corr_matrix", "cov_matrix", "positive_ordered", "array"
  ) %>%
    str_c(collapse = "|") %>%
    str_c("(", ., ")")

  pattern <- str_c(type, "[<[^>]+>]*\\s+(\\w+)(\\[\\w+(,\\w+)*\\])?$")
  x %<>% magrittr::extract(str_detect(x, pattern))

  !str_detect(x, "\\[")
}


extract_types <- function(x, block_location) {
  x %<>% str_sub(block_location$start, block_location$end) %>%
    str_trim() %>%
    str_replace_all("([{]|[}])", "") %>%
    str_trim() %>%
    str_replace(";$", "") %>%
    str_split(";", simplify = TRUE) %>%
    str_trim()
  x

  type <- c(
    "int", "real", "vector", "simplex", "ordered", "row_vector",
    "matrix", "corr_matrix", "cov_matrix", "positive_ordered", "array"
  ) %>%
    str_c(collapse = "|") %>%
    str_c("(", ., ")")

  pattern <- str_c(type, "[<[^>]+>]*?\\s+(\\w+)$")
  x %<>% magrittr::extract(str_detect(x, pattern))

  types <- str_extract(x, type)

  types
}


get_par_names <- function(x, block_name = "parameters") {
  x %<>% rm_comments() %>% clean_blocks()
  block_location <- get_block_location(x, block_name)
  extract_pars(x, block_location)
}

has_block <- function(x, block_name) {
  length(get_block_location(x, block_name))
}

get_par_scalar <- function(x, block_name = "parameters") {
  x %<>% rm_comments() %>% clean_blocks()
  block_location <- get_block_location(x, block_name)
  extract_scalar(x, block_location)
}

get_par_types <- function(x, block_name = "parameters") {
  x %<>% rm_comments() %>% clean_blocks()
  block_location <- get_block_location(x, block_name)
  extract_types(x, block_location)
}

get_par_type <- function(x, parameter, block_name = "parameters") {
  get_par_types(x, block_name)[get_par_names(x, block_name) == parameter]
}

paste_transformed_data <- function(x, text, top = TRUE) {
  text %<>% rm_comments()

  if (!has_block(x, "transformed data")) {
    x %<>% str_replace(
      "\\n\\s*parameters\\s*[{]",
      "\ntransformed data{\n}\nparameters{"
    )
  }

  if (top) {
    text %<>% str_c("\ntransformed data{\n", .)
    x %<>% str_replace("\\n\\s*transformed data\\s*[{]", text)
  } else {
    text %<>% str_c("\n}\nparameters{")
    x %<>% str_replace("[}]\\s*\\n\\s*parameters\\s*[{]", text)
  }
  x
}
