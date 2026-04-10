# Changelog

## smbr 0.0.1.9012

- Added following addition parameter types
  - “cholesky_factor_cov”
  - “cholesky_factor_corr”
  - “unit_vector”
  - “sum_to_zero_vector”
  - “sum_to_zero_matrix”

## smbr 0.0.1.9011

- Added `seed` and `niters_warmup` arguments to analyse1.smb_model.

## smbr 0.0.1.9010

- Removed dependency on rstan branch.

## smbr 0.0.1.9009

- Fixed bug in pars caused by latest changes in embr.

## smbr 0.0.1.9008

- adding StanHeaders

## smbr 0.0.1.9007

- Added
  [`data_block()`](https://poissonconsulting.github.io/smbr/reference/data_block.md)
  function.
- Fixed GHA.
- Fixed broken tests.

## smbr 0.0.1.9006

- Transformed parameters are now only monitored if named in derived.

## smbr 0.0.1.9005

- Added `sd_priors_by()`.

## smbr 0.0.1

- Added a `NEWS.md` file to track changes to the package.
