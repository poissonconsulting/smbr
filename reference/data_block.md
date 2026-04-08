# Create Data Block from Data List

Automate writing the STAN data block from your data.

## Usage

``` r
data_block(x)
```

## Arguments

- x:

  A nlist

## Value

A string

## Examples

``` r
mod_data <- nlist::as_nlist(list(
  X = c(1L, 2L, 3L, 4L),
  Y = c(1.2, 7.3, 8.9, 2.6),
  nObs = 4L
))
data_block(mod_data)
#> [1] "data {\n  int X[nObs];\n  real Y[nObs];\n  int nObs;\n}"
```
