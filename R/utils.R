# should be more stable than log(colMeans(exp(x)))
logColMeansExp <- function(x) {
  matrixStats::colLogSumExps(x) - log(nrow(x))
}

parameters_arg2to1 <- function(param_type, x, scalar_only) {
  parameters(x = x, param_type = param_type, scalar_only = scalar_only)
}
