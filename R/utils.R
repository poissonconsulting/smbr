# 0. delete comments
# 1. get black
# 2. get lines within blocks
# 3. find parameter definitions
# 4. extract name
source("Stan-example.R")
x %<>% mb_code()
y <- x

x %<>% remove_comments()
x %<>% str_replace_all(";.*\n", ";\n")

remove_comments <- function(x) {

  x %<>% str_replace_all("//.*", "")

  x

}

#get_block <- function(x, block_name) {
  if(!str_detect(x, block_name)) return(character(0))

  possible_blocks <- c("data", "transformed data", "parameters",
                       "transformed parameters", "model", "generated quantities")

  p <- str_c(c(possible_blocks, "$"), collapse = "|") # possible patterns after curly bracket
  #p <- str_c(c(possible_blocks), collapse = "|") # possible patterns after curly bracket

  x %<>% str_replace_all("\\n", " ") %>%
    str_replace_all("\\s{2,} ", " ")

  pattern <- str_c("(^|[}])\\s*", block_name, "\\s*[{].*[}]\\s*[(", p, ")]?")

  #str_detect(x, pattern)
  x %<>% str_extract_all(pattern, simplify = TRUE); x

  block_name <- "model"
block_name <- "generated quantities"
  x %<>%
    str_replace(str_c("(^|[}])\\s*", block_name, "\\s*[{]\\s*"), "") %<>%
    str_replace(";\\s*[}]", "")

  if (length(x) > 1) stop(base::sprintf("multiple '%s' blocks in model", block_name))

  str_(x, str_c("(^|[}])\\s*", block_name, "\\s*[{].*[}]"))

  x %<>% str_replace(str_c("(.*[}]\\s{0,}", block_name, "\\s{0,1}[{])([^}]*)(.*)"), "\\2") %>%
    str_replace("\\s$", "")

  x %<>% str_split(";") %>% unlist() %>% purrr::discard(function(x) identical(x, ""))

  x
}

get_block(x, "parameters")
