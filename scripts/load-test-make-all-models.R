load_all(".")

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
      // real mu_y2; // unmodeled param
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

model <- model(code = template, fixed = "_y$")

expect_identical(class(model), c("smb_model", "mb_model"))
expect_true(is.smb_model(model))

expect_identical(pars(model), c("foo", "mu_y", "tau_y"))
expect_identical(pars(code(model)), c("bar", "foo", "mu_y", "sigma_y", "tau_y"))

expect_identical(
  pars(model, "primary"),
  c("foo", "mu_y", "tau_y")
)

expect_identical(pars(model, "primary", scalar = TRUE), c("mu_y", "tau_y"))

model <- update_model(model, drops = list("mu_y", "tau_y"))

models <- make_all_models(model)

expect_identical(models[[1]], model)
