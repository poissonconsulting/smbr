as.mcmcr.stanfit <- function(x, ...) {

  pex <- rstan::extract(x) # permuted extract
  ppars <- names(pex) # permuted pars
  ppars <- ppars[!(ppars %in% "lp__")]

  ex <- rstan::extract(x, permute = FALSE) # unpermuted extract
  pars <- attr(ex, "dimnames")$parameters
  pars <- pars[!(pars %in% "lp__")]

  # Distingish scalars and vectors
  count_scalar <- sapply(ppars, function(X) {
    stringr::str_detect(pars, stringr::str_c("^", X, "$")) %>%
      which() %>% length()
  })

  count_vector <- sapply(ppars, function(X) {
    stringr::str_detect(pars, stringr::str_c("^", X, "\\[[0-9]+\\]$")) %>%
      which() %>% length()
  })

  par_n <- count_scalar + count_vector
  which_scalar <- which(count_scalar > 0)
  which_vector <- which(count_vector > 0)

  # All count_scalar == 0, count_vector > 0 & vice verse
  stopifnot(all(count_scalar * count_vector == 0))

  mcmcr <- base::vector(mode = "list", length = length(ppars))
  for (i in 1:length(mcmcr)) {
    exi <- rstan::extract(x, pars = ppars[i], permute = FALSE)
    dim(exi)

    mcmcr[[i]] <- array(dim = c(npars = dim(exi)[3], niters = dim(exi)[1],
                                nchains = dim(exi)[2]))
    dim(mcmcr[[i]])
    for (j in 1:dim(exi)[3]) mcmcr[[i]][j, , ] <- exi[, , j]
    class(mcmcr[[i]]) <- "mcarray"
  }
  mcmcr %<>% mcmcr::as.mcmcr()
  names(mcmcr) <- ppars

  mcmcr

}
