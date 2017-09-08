
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
      int nAnnual;
      int nObs;
      int Annual[nObs];
      int Pairs[nObs];
      real Year[nObs];
  }
  parameters {
      vector[nAnnual] bAnnual;
      real log_sAnnual;
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
      vector[nObs] ePairs;
    
      log_sAnnual ~ normal(0, 10);
      bAnnual ~ normal(0, sAnnual);

      alpha ~ normal(0, 10);
      beta1 ~ normal(0, 10);
      beta2 ~ normal(0, 10);
      beta3 ~ normal(0, 10);

      for (i in 1:nObs) {
        ePairs[i] = exp(alpha + beta1 * Year[i] + beta2 * Year[i]^2 + 
                      beta3 * Year[i]^3 + bAnnual[Annual[i]]);
      }
      target += poisson_lpmf(Pairs | ePairs);
  }")

# add R code to calculate derived parameters
model %<>% update_model(new_expr = "
  for (i in 1:length(Pairs)) {
    prediction[i] <- exp(alpha + beta1 * Year[i] + beta2 * Year[i]^2 + 
                       beta3 * Year[i]^3 + bAnnual[Annual[i]])
  }
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
#> 1    40     5     2000       4  4000           2.2s  1.01      TRUE

# coefficient table
coef(analysis)
#> # A tibble: 5 x 7
#>          term    estimate         sd      zscore       lower      upper
#> *  <S3: term>       <dbl>      <dbl>       <dbl>       <dbl>      <dbl>
#> 1       alpha  4.21004553 0.04020725 104.7204450  4.13383791  4.2875014
#> 2       beta1  1.19633995 0.07541715  15.9156836  1.06499179  1.3610243
#> 3       beta2  0.01611315 0.03120187   0.5494325 -0.04275364  0.0797373
#> 4       beta3 -0.27386261 0.03862127  -7.1308844 -0.35388962 -0.2028511
#> 5 log_sAnnual -2.21937108 0.30324436  -7.4095614 -2.89592754 -1.7204897
#> # ... with 1 more variables: pvalue <dbl>

# trace plots
plot(analysis)
```

![](tools/README-unnamed-chunk-3-1.png)![](tools/README-unnamed-chunk-3-2.png)

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

-   [smbr](http://www.poissonconsulting.ca/smbr/)
