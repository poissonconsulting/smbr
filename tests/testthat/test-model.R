context("parameters")

test_that("parameters", {

  template <- ("
    data {
      int<lower=0> N; // unmodeled data
      real y[N]; // modeled data
      real mu_mu; // config. unmodeled param
      real<lower=0> sigma_mu; // config. unmodeled param
    }
    transformed data {
      real<lower=0> alpha; // const. unmodeled param
      real<lower=0> beta; // const. unmodeled param
      alpha = 0.1;
      beta = 0.1;
    }
    parameters {
      real mu_y; // modeled param
      real<lower=0> tau_y; // modeled param
      vector<lower=0>[N] foo; // just for testing purposes
    }
    transformed parameters {
      real<lower=0> sigma_y; // derived quantity (param)
      sigma_y = pow(tau_y, -0.5);
      vector<lower=0>[N] bar; // just for testing purposes
    }
    model {
      tau_y ~ gamma(alpha, beta);
      mu_y ~ normal(mu_mu, sigma_mu);
      for (n in 1:N) {
        y[n] ~ normal(mu_y, sigma_y);
      }
    }
    generated quantities {
      real variance_y; // derived quantity (transform)
      variance_y = sigma_y * sigma_y;
    }")

  code <- mbr::mb_code(template)
  model <- mbr::model(template)

  # Test that classes are correct
  expect_true(inherits(code, c("smb_code", "mb_code")))
  expect_false(inherits(code, "jmb_code"))
  expect_false(inherits(code, "lmb_code"))
  expect_false(inherits(code, "tmb_code"))

  expect_true(inherits(model, c("smb_model", "mb_model")))
  expect_false(inherits(model, "jmb_model"))
  expect_false(inherits(model, "lmb_model"))
  expect_false(inherits(model, "tmb_model"))

  # Test that parameters are correct
  expect_identical(parameters(model$code), c("mu_y", "tau_y"))
  expect_identical(parameters(model$code, scalar = FALSE),
                   c("foo", "mu_y", "tau_y"))

  expect_identical(parameters(model$code, param_type = "derived"),
                   c("sigma_y", "variance_y"))
  expect_identical(parameters(model$code, param_type = "derived",
                              scalar = FALSE),
                   c("bar", "sigma_y", "variance_y"))

  #expect_identical(parameters(mb_code(template)),
  #                 c("bAdultsInitial", "bDisturbance", "bHunterDays", "bPDO", "bSurvival",
  #                   "bYearlingsInitial", "log_sMales"))

#  expect_identical(parameters(mb_code(template), "derived"),
 #                  "sMales")

  expect_error(parameters(mb_code(template), "adreport"))
  expect_error(parameters(mb_code(template), "report"))
})
