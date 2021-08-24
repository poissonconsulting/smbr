test_that("analyse", {
    mbr::set_analysis_mode("check")

  # define model in Stan language
  model <- mbr::model("
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
      Pairs ~ poisson(ePairs);
  }")

  # add R code to calculate derived parameters
  model <- mbr::update_model(model, new_expr = "
  for (i in 1:length(Pairs)) {
    prediction[i] <- exp(alpha + beta1 * Year[i] + beta2 * Year[i]^2 +
                       beta3 * Year[i]^3 + bAnnual[Annual[i]])
  }
    log_lik <- dpois(Pairs, prediction, log = TRUE)")

  # define data types and center year
  model <- mbr::update_model(model,
                        select_data = list("Pairs" = integer(), "Year*" = integer(),
                                           Annual = factor()),
                        derived = "sAnnual",
                        random_effects = list(bAnnual = "Annual"),
                        gen_inits = function(data) { list(log_sAnnual =  20) })

  data <- bauw::peregrine
  data$Annual <- factor(data$Year)

  set.seed(34)

  # analyse
  analysis <- mbr::analyse(model, data = data)

  expect_identical(class(analysis), c("smb_analysis", "mb_analysis"))
  expect_true(is.smb_analysis(analysis))

  expect_identical(universals::niters(analysis), 500L)
  expect_identical(universals::nchains(analysis), 2L)
  expect_identical(universals::nsims(analysis), 1000L)
  expect_identical(mbr::ngens(analysis), 2000L)

  analysis <- reanalyse(analysis)

  expect_identical(niters(analysis), 500L)
  expect_identical(ngens(analysis), 4000L)

  expect_identical(pars(analysis, "fixed"), sort(c("alpha", "beta1", "beta2", "beta3", "log_sAnnual")))
  expect_identical(pars(analysis, "random"), "bAnnual")
  expect_identical(pars(analysis), sort(c("alpha", "bAnnual", "beta1", "beta2", "beta3", "log_sAnnual", "sAnnual")))
  expect_identical(pars(analysis, "primary"), sort(c("alpha", "bAnnual", "beta1", "beta2", "beta3", "log_sAnnual")))
  expect_error(pars(analysis, "some"))

  expect_is(as.mcmcr(analysis), "mcmcr")

  monitor <- rstan::monitor(analysis$stanfit, print = FALSE)
  rhat <- rhat(analysis, by = "term", as_df = TRUE)

  rhat_stan <- tibble::tibble(term = as.term(row.names(monitor)), rhat = round(monitor[,"Rhat"], 3))
  rhat_stan <- rhat_stan[rhat_stan$term != "lp__",]
  expect_identical(rhat$term, rhat_stan$term)

  glance <- glance(analysis)
  expect_is(glance, "tbl")
  expect_identical(glance$n, 40L)
  expect_identical(glance$K, 5L)
  expect_identical(glance$nthin, 2L)

    expect_identical(colnames(glance), c("n", "K", "logLik", "IC", "nchains", "niters",  "nthin", "ess", "rhat", "converged"))

  waic <- IC(analysis)
  expect_gt(waic, 305)
  expect_lt(waic, 315)

  coef <- coef(analysis, simplify = TRUE)

  expect_is(coef, "tbl")
  expect_is(coef, "mb_analysis_coef")
  expect_identical(colnames(coef), c("term", "estimate", "lower", "upper", "svalue"))

  expect_identical(coef$term, sort(as.term(c("alpha", "beta1", "beta2", "beta3", "log_sAnnual"))))

  expect_identical(coef(analysis, "derived")$term, as.term("sAnnual"))
  expect_identical(coef(analysis, "all")$term, sort(as.term(c("alpha", paste0("bAnnual[", 1:40,"]"), "beta1", "beta2", "beta3", "log_sAnnual", "sAnnual"))))

  tidy <- tidy(analysis)
  expect_identical(colnames(tidy), c("term", "estimate", "lower", "upper", "esr", "rhat"))

  year <- predict(analysis, new_data = "Year")

  expect_is(year, "tbl")
  expect_identical(colnames(year), c("Year", "Pairs", "R.Pairs", "Eyasses", "Annual",
                                     "estimate", "sd", "zscore", "lower", "upper", "pvalue"))
  expect_true(all(year$estimate > year$lower))
  expect_true(all(year$estimate < year$upper))

  expect_equal(unlist(estimates(analysis)), coef$estimate, check.names = FALSE)
})
