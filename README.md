
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![stability-unstable](https://img.shields.io/badge/stability-unstable-yellow.svg)](https://github.com/joethorley/stability-badges#unstable) [![Travis-CI Build Status](https://travis-ci.org/poissonconsulting/smbr.svg?branch=master)](https://travis-ci.org/poissonconsulting/smbr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/poissonconsulting/smbr?branch=master&svg=true)](https://ci.appveyor.com/project/poissonconsulting/smbr) [![codecov](https://codecov.io/gh/poissonconsulting/smbr/branch/master/graph/badge.svg)](https://codecov.io/gh/poissonconsulting/smbr) [![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/mbr)](https://cran.r-project.org/package=mbr)

smbr
====

Introduction
------------

`smbr` (pronounced simber) is an R package to facilitate analyses using [STAN](http://mc-stan.org/about/). It is part of the [mbr](https://github.com/poissonconsulting/mbr) family of packages.

Demonstration
-------------

``` r
library(bauw)
library(ggplot2)
library(magrittr)
library(smbr)
```

``` r
# define model in Stan language
model <- model("
  data {
    
    // Data lengths
      int nAnnual;
      int nObs;

    // Data
      int Annual[nObs];
      int Pairs[nObs];
      real Year[nObs];

  }
  parameters {

    // Random effects
      vector[nAnnual] bAnnual;
      real log_sAnnual;

    // Regression model
      real alpha;
      real beta1;
      real beta2;
      real beta3;

  }
  transformed parameters {
  
    real sAnnual;
    sAnnual = exp(log_sAnnual);

  }
  model {

    // Vectors of expected values
      vector[nObs] ePairs;
    
    // Priors on random effects
      log_sAnnual ~ normal(0, 10);
      bAnnual ~ normal(0, sAnnual);

    // Priors on regression coefficients
      alpha ~ normal(0, 10);
      beta1 ~ normal(0, 10);
      beta2 ~ normal(0, 10);
      beta3 ~ normal(0, 10);

    // Model
      for (i in 1:nObs) {
        ePairs[i] = exp(alpha + beta1 * Year[i] + beta2 * Year[i]^2 + 
                      beta3 * Year[i]^3 + bAnnual[Annual[i]]);
      }

      target += poisson_lpmf(Pairs | ePairs);

  }")

# add R code to modify data before running Stan
model %<>% update_model(modify_data = function(data) {
  data$nObs <- length(data$Pairs)
  data$Annual %<>% as.integer()
  data
})

# add R code to calculate derived parameters
model %<>% update_model(new_expr = "
  for (i in 1:length(Pairs)) {
    prediction[i] <- exp(alpha + beta1 * Year[i] + beta2 * Year[i]^2 + 
                       beta3 * Year[i]^3 + bAnnual[Annual[i]])
  }
  
  log_lik <- dpois(Pairs, prediction, log = TRUE)

  ")

# define data types and center year
model %<>% update_model(
  select_data = list("Pairs" = integer(), "Year*" = integer(), 
                     Annual = factor()),
  derived = "sAnnual",
  random_effects = list(bAnnual = "Annual"))

data <- bauw::peregrine
data$Annual <- factor(data$Year)

# analyse
analysis <- analyse(model, data = data)
#> # A tibble: 1 x 8
#>       n     K nsamples nchains nsims       duration  rhat converged
#>   <int> <int>    <int>   <int> <int> <S4: Duration> <dbl>     <lgl>
#> 1    40     5     2000       4  4000           2.1s  1.01      TRUE
analysis %<>% reanalyse(rhat = 1.05)
#> # A tibble: 1 x 8
#>       n     K nsamples nchains nsims       duration  rhat converged
#>   <int> <int>    <int>   <int> <int> <S4: Duration> <dbl>     <lgl>
#> 1    40     5     2000       4  4000           2.1s  1.01      TRUE

# coefficient table
coef(analysis)
#> # A tibble: 5 x 7
#>          term   estimate         sd     zscore       lower       upper
#> *  <S3: term>      <dbl>      <dbl>      <dbl>       <dbl>       <dbl>
#> 1       alpha  4.2118153 0.04361616 96.5757903  4.12565786  4.29793711
#> 2       beta1  1.1957392 0.07244861 16.5566923  1.06875266  1.35430005
#> 3       beta2  0.0149033 0.03267574  0.4541163 -0.04797471  0.07864761
#> 4       beta3 -0.2729313 0.03745022 -7.3406008 -0.35354174 -0.20688109
#> 5 log_sAnnual -2.2363485 0.27751665 -8.0912262 -2.82332558 -1.73190190
#> # ... with 1 more variables: pvalue <dbl>

# trace plots
plot(analysis)
```

![](tools/README-unnamed-chunk-3-1.png)![](tools/README-unnamed-chunk-3-2.png)

``` r

# widely applicable information criterion (WAIC)
IC(analysis) # should be about 309.5
#> # A tibble: 1 x 7
#>       n   elpd se.elpd     p  se.p  waic se.waic
#>   <int>  <dbl>   <dbl> <dbl> <dbl> <dbl>   <dbl>
#> 1    40 -154.3     4.3  16.4   2.4 308.5     8.6
```

``` r
# make predictions by varying year with other predictors including the random effect of Annual held constant
year <- predict(analysis, new_data = "Year")

# plot those predictions
ggplot(data = year, aes(x = Year, y = estimate)) +
  geom_point(data = bauw::peregrine, aes(y = Pairs)) +
  geom_line() +
  geom_line(aes(y = lower), linetype = "dotted") +
  geom_line(aes(y = upper), linetype = "dotted") +
  expand_limits(y = 0)
```

![](tools/README-unnamed-chunk-4-1.png)

``` r

# Compare models using information criteria

## Define simpler model for comparison

simpler_model <- model("
  data {
    
    // Data lengths
      int nObs;

    // Data
      int Pairs[nObs];
      real Year[nObs];

  }
  parameters {

    // Regression model
      real alpha;
      real beta1;
      real beta2;
      real beta3;

  }
  model {

    // Vectors of expected values
      vector[nObs] ePairs;
    
    // Priors on regression coefficients
      alpha ~ normal(0, 10);
      beta1 ~ normal(0, 10);
      beta2 ~ normal(0, 10);
      beta3 ~ normal(0, 10);

    // Model
      for (i in 1:nObs) {
        ePairs[i] = exp(alpha + beta1 * Year[i] + beta2 * Year[i]^2 + 
                      beta3 * Year[i]^3);
      }

      target += poisson_lpmf(Pairs | ePairs);

  }")

# add R code to calculate derived parameters
simpler_model %<>% update_model(new_expr = "
  for (i in 1:length(Pairs)) {
    prediction[i] <- exp(alpha + beta1 * Year[i] + beta2 * Year[i]^2 + 
                       beta3 * Year[i]^3)
  }
  
  log_lik <- dpois(Pairs, prediction, log = TRUE)

  ")

# modify data, define data types, and center year
simpler_model %<>% update_model(modify_data = model$modify_data)
simpler_model %<>% update_model(
  select_data = list("Pairs" = integer(), "Year*" = integer()))

# analyse
analyses <- analyse(models(model, simpler_model), data = data)
#> Model: 1 
#> # A tibble: 1 x 8
#>       n     K nsamples nchains nsims       duration  rhat converged
#>   <int> <int>    <int>   <int> <int> <S4: Duration> <dbl>     <lgl>
#> 1    40     5     2000       4  4000           2.1s  1.03      TRUE
#> Model: 2 
#> # A tibble: 1 x 8
#>       n     K nsamples nchains nsims       duration  rhat converged
#>   <int> <int>    <int>   <int> <int> <S4: Duration> <dbl>     <lgl>
#> 1    40     4     2000       4  4000           0.8s  1.01      TRUE
analyses %<>% reanalyse(rhat = 1.05)
#> Model: 1 
#> # A tibble: 1 x 8
#>       n     K nsamples nchains nsims       duration  rhat converged
#>   <int> <int>    <int>   <int> <int> <S4: Duration> <dbl>     <lgl>
#> 1    40     5     2000       4  4000           2.1s  1.03      TRUE
#> Model: 2 
#> # A tibble: 1 x 8
#>       n     K nsamples nchains nsims       duration  rhat converged
#>   <int> <int>    <int>   <int> <int> <S4: Duration> <dbl>     <lgl>
#> 1    40     4     2000       4  4000           0.8s  1.01      TRUE

# glance at analyses
glance(analyses)
#> [[1]]
#> # A tibble: 1 x 8
#>       n     K nsamples nchains nsims       duration  rhat converged
#>   <int> <int>    <int>   <int> <int> <S4: Duration> <dbl>     <lgl>
#> 1    40     5     2000       4  4000           2.1s  1.03      TRUE
#> 
#> [[2]]
#> # A tibble: 1 x 8
#>       n     K nsamples nchains nsims       duration  rhat converged
#>   <int> <int>    <int>   <int> <int> <S4: Duration> <dbl>     <lgl>
#> 1    40     4     2000       4  4000           0.8s  1.01      TRUE

# widely applicable information criterion (WAIC)
#IC(analyses)
```

Installation
------------

``` r
# install.packages("devtools")
devtools::install_github("poissonconsulting/smbr")
```

Contribution
------------

Please report any [issues](https://github.com/poissonconsulting/smbr/issues).

[Pull requests](https://github.com/poissonconsulting/smbr/pulls) are always welcome.

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

Inspiration
-----------

-   [jaggernaut](https://github.com/poissonconsulting/jaggernaut)

Creditation
-----------

-   [STAN](http://mc-stan.org)

Documentation
-------------

-   [STAN Documentation](http://mc-stan.org/users/documentation/)
