test_that("code", {
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
      // real mu_y2; // modeled param
      real mu_y; // modeled param
      real<lower=0> tau_y; // modeled param
      vector<lower=0>[N] foo; // just for testing purposes
      real<lower=0> foo2[N,N]; // just for testing purposes
    }
    transformed parameters {
      real<lower=0> sigma_y; // derived quantity (param)
      sigma_y = pow(tau_y, -0.5);
      vector<lower=0>[N] bar; // just for testing purposes
    }
    model {
      sigma_mu ~ normal(0, 2);
      sigma_y ~ normal(1.1, theta);
      sigma_y ~ lognormal(alpha, 2.20);
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

  code <- mb_code(template)

  expect_identical(class(code), c("smb_code", "mb_code"))
  expect_true(is.smb_code(code))

  expect_identical(pars(code), c("bar", "foo", "foo2", "mu_y", "sigma_y", "tau_y"))

  expect_identical(
    pars(code, "primary"),
    c("foo", "foo2", "mu_y", "tau_y")
  )

  expect_identical(pars(code, "primary", scalar = TRUE), c("mu_y", "tau_y"))

  expect_identical(
    pars(code, param_type = "derived"),
    c("bar", "sigma_y")
  )
  expect_identical(
    pars(code, param_type = "derived", scalar = TRUE),
    c("sigma_y")
  )

  code10 <- sd_priors_by(code, 10, distributions = c("logistic", "normal", "lognormal", "t"))
  expect_true(is.smb_code(code10))
  expect_identical(pars(code10), pars(code))
  expect_identical(
    as.character(code10),
    "\n    data {\n      int<lower=0> N;\n      real y[N];\n      real mu_mu;\n      real<lower=0> sigma_mu;\n    }\n    transformed data {\n      real<lower=0> alpha;\n      real<lower=0> beta;\n      alpha = 0.1;\n      beta = 0.1;\n    }\n    parameters {\n      real mu_y;\n      real<lower=0> tau_y;\n      vector<lower=0>[N] foo;\n      real<lower=0> foo2[N,N];\n    }\n    transformed parameters {\n      real<lower=0> sigma_y;\n      sigma_y = pow(tau_y, -0.5);\n      vector<lower=0>[N] bar;\n    }\n    model {\n      sigma_mu ~ normal(0, 2 * 10);\n      sigma_y ~ normal(1.1, theta);\n      sigma_y ~ lognormal(alpha, 2.20 * 10);\n      tau_y ~ gamma(alpha, beta);\n      mu_y ~ normal(mu_mu, sigma_mu);\n      for (n in 1:N) {\n        y[n] ~ normal(mu_y, sigma_y);\n      }\n    }\n    generated quantities {\n      real variance_y;\n      variance_y = sigma_y * sigma_y;\n    }"
  )

  coder <- sd_priors_by(code, 2, distributions = "normal")
  expect_true(is.smb_code(coder))
  expect_identical(pars(coder), pars(code))
  expect_identical(
    as.character(coder),
    "\n    data {\n      int<lower=0> N;\n      real y[N];\n      real mu_mu;\n      real<lower=0> sigma_mu;\n    }\n    transformed data {\n      real<lower=0> alpha;\n      real<lower=0> beta;\n      alpha = 0.1;\n      beta = 0.1;\n    }\n    parameters {\n      real mu_y;\n      real<lower=0> tau_y;\n      vector<lower=0>[N] foo;\n      real<lower=0> foo2[N,N];\n    }\n    transformed parameters {\n      real<lower=0> sigma_y;\n      sigma_y = pow(tau_y, -0.5);\n      vector<lower=0>[N] bar;\n    }\n    model {\n      sigma_mu ~ normal(0, 2 * 2);\n      sigma_y ~ normal(1.1, theta);\n      sigma_y ~ lognormal(alpha, 2.20);\n      tau_y ~ gamma(alpha, beta);\n      mu_y ~ normal(mu_mu, sigma_mu);\n      for (n in 1:N) {\n        y[n] ~ normal(mu_y, sigma_y);\n      }\n    }\n    generated quantities {\n      real variance_y;\n      variance_y = sigma_y * sigma_y;\n    }"
  )
})
