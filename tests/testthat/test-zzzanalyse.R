context("analyse")

test_that("analyse", {
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
  model <- update_model(model, new_expr = "
  for (i in 1:length(Pairs)) {
    prediction[i] <- exp(alpha + beta1 * Year[i] + beta2 * Year[i]^2 +
                       beta3 * Year[i]^3 + bAnnual[Annual[i]])
  }
    logLik <- dpois(Pairs, prediction, log = TRUE)")

  # define data types and center year
  model <- update_model(model,
                        select_data = list("Pairs" = integer(), "Year*" = integer(),
                                           Annual = factor()),
                        derived = "sAnnual",
                        random_effects = list(bAnnual = "Annual"))

  data <- bauw::peregrine
  data$Annual <- factor(data$Year)

  # analyse
  analysis <- analyse(model, data = data, parallel = FALSE,
                      glance = FALSE, beep = FALSE,
                      quiet = TRUE)

  expect_identical(ngens(analysis), 1000L)
  expect_identical(nsims(analysis), 4000L)

  expect_identical(niters(analysis), 500L)
  expect_identical(nchains(analysis), 4L)
  expect_identical(nsamples(analysis), 2000L)

  expect_identical(class(analysis), c("smb_analysis", "mb_analysis"))
  expect_true(is.smb_analysis(analysis))

  analysis <- reanalyse(analysis, beep = FALSE, glance = FALSE, parallel = FALSE, quiet = TRUE, rhat = 1.0)

  expect_identical(parameters(analysis, "fixed"), sort(c("alpha", "beta1", "beta2", "beta3", "log_sAnnual")))
  expect_identical(parameters(analysis, "random"), "bAnnual")
  expect_identical(parameters(analysis), sort(c("alpha", "bAnnual", "beta1", "beta2", "beta3", "log_sAnnual", "sAnnual")))
  expect_identical(parameters(analysis, "primary"), sort(c("alpha", "bAnnual", "beta1", "beta2", "beta3", "log_sAnnual")))
  expect_error(parameters(analysis, "some"))

  expect_identical(ngens(analysis), 2000L)
  expect_identical(nsims(analysis), 8000L)

  expect_identical(niters(analysis), 500L)
  expect_identical(nchains(analysis), 4L)
  expect_identical(nsamples(analysis), 2000L)

  expect_is(as.mcmcr(analysis), "mcmcr")

  glance <- glance(analysis)
  expect_is(glance, "tbl")
  expect_identical(colnames(glance), c("n", "K", "nsamples", "nchains", "nsims", "duration", "rhat", "converged"))
  expect_is(glance$duration, "Duration")
  expect_identical(glance$n, 40L)
  expect_identical(glance$K, 5L)

  waic <- IC(analysis)
  expect_gt(waic, 305)
  expect_lt(waic, 315)

  coef <- coef(analysis)

  expect_is(coef, "tbl")
  expect_is(coef, "mb_analysis_coef")
  expect_identical(colnames(coef), c("term", "estimate", "sd", "zscore", "lower", "upper", "pvalue"))

  expect_identical(coef$term, sort(as.term(c("alpha", "beta1", "beta2", "beta3", "log_sAnnual"))))

  expect_identical(coef(analysis, "derived")$term, as.term("sAnnual"))
  expect_identical(coef(analysis, "all")$term, sort(as.term(c("alpha", paste0("bAnnual[", 1:40,"]"), "beta1", "beta2", "beta3", "log_sAnnual", "sAnnual"))))

  tidy <- tidy(analysis)
  expect_identical(colnames(tidy), c("term", "estimate", "std.error", "statistic", "p.value"))
  expect_identical(tidy$estimate, coef$estimate)

  year <- predict(analysis, new_data = "Year")

  expect_is(year, "tbl")
  expect_identical(colnames(year), c("Year", "Pairs", "R.Pairs", "Eyasses", "Annual",
                                     "estimate", "sd", "zscore", "lower", "upper", "pvalue"))
  expect_true(all(year$estimate > year$lower))
  expect_true(all(year$estimate < year$upper))

  expect_equal(unlist(estimates(analysis)), coef$estimate, check.names = FALSE)
})
