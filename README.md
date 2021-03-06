
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R build
status](https://github.com/poissonconsulting/smbr/workflows/R-CMD-check/badge.svg)](https://github.com/poissonconsulting/smbr/actions)
[![Codecov test
coverage](https://codecov.io/gh/poissonconsulting/smbr/branch/master/graph/badge.svg)](https://codecov.io/gh/poissonconsulting/smbr?branch=master)
[![License:
MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1162382.svg)](https://doi.org/10.5281/zenodo.1162382)

# smbr

## Introduction

`smbr` (pronounced simber) is an R package to facilitate analyses using
[`STAN`](http://mc-stan.org). It is part of the
[mbr](https://github.com/poissonconsulting/mbr) family of packages.

## Demonstration

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

set.seed(42)

# analyse
analysis <- analyse(model, data = data, seed = 3L, glance = FALSE)

# coefficient table
coef(analysis)
#> Warning: The `simplify` argument of `coef()` must be TRUE as of mcmcr 0.4.1.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_warnings()` to see where this warning was generated.
#> # A tibble: 5 x 7
#>   term        estimate     sd  zscore   lower   upper   pvalue
#>   <term>         <dbl>  <dbl>   <dbl>   <dbl>   <dbl>    <dbl>
#> 1 alpha         4.26   0.0400 107.     4.19    4.34   0.000333
#> 2 beta1         1.18   0.0771  15.4    1.05    1.35   0.000333
#> 3 beta2        -0.0189 0.0301  -0.644 -0.0787  0.0411 0.492   
#> 4 beta3        -0.267  0.0394  -6.82  -0.351  -0.199  0.000333
#> 5 log_sAnnual  -2.26   0.716   -3.47  -4.51   -1.75   0.000333

# trace plots
plot(analysis)
```

![](tools/README-unnamed-chunk-3-1.png)<!-- -->![](tools/README-unnamed-chunk-3-2.png)<!-- -->

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

![](tools/README-unnamed-chunk-4-1.png)<!-- -->

## Installation

``` r
# install.packages("devtools")
devtools::install_github("poissonconsulting/smbr")
```

## Citation


    To cite smbr in publications use:

      Chris Muir and Joe Thorley (2018) smbr: Analyses Using STAN. doi:
      https://doi.org/10.5281/zenodo.1162382.

    A BibTeX entry for LaTeX users is

      @Misc{,
        author = {Chris Muir and Joe Thorley},
        year = {2018},
        title = {smbr: Analyses Using STAN},
        doi = {https://doi.org/10.5281/zenodo.1162382},
      }

    Please also cite STAN.

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/smbr/issues).

[Pull requests](https://github.com/poissonconsulting/smbr/pulls) are
always welcome.

## Code of Conduct

Please note that the smbr project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
