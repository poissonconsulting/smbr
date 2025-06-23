test_that("analyse", {
  embr::set_analysis_mode("check")

  # define model in Stan language
  model <- embr::model(mb_code("
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
    real eAnnual;
    sAnnual = exp(log_sAnnual);
    eAnnual = exp(log_sAnnual);
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
  }"))

  # add R code to calculate derived parameters
  model <- embr::update_model(model, new_expr = "
  for (i in 1:length(Pairs)) {
    prediction[i] <- exp(alpha + beta1 * Year[i] + beta2 * Year[i]^2 +
                       beta3 * Year[i]^3 + bAnnual[Annual[i]])
  }
    log_lik <- dpois(Pairs, prediction, log = TRUE)")

  # define data types and center year
  model <- embr::update_model(model,
    select_data = list(
      "Pairs" = integer(), "Year*" = integer(),
      Annual = factor()
    ),
    derived = "sAnnual",
    random_effects = list(bAnnual = "Annual"),
    gen_inits = function(data) {
      list(log_sAnnual = 20)
    }
  )

  expect_identical(
    pars(model, "fixed"),
    c("alpha", "beta1", "beta2", "beta3", "log_sAnnual")
  )
  expect_identical(pars(model, "random"), "bAnnual")
  expect_identical(
    pars(model, "primary"),
    c("alpha", "bAnnual", "beta1", "beta2", "beta3", "log_sAnnual")
  )
  expect_identical(pars(model, "derived"), "sAnnual")

  expect_identical(
    pars(model, "all"),
    c("alpha", "bAnnual", "beta1", "beta2", "beta3", "log_sAnnual", "sAnnual")
  )

  expect_identical(
    pars(code(model), "all"),
    c("alpha", "bAnnual", "beta1", "beta2", "beta3", "eAnnual", "log_sAnnual", "sAnnual")
  )

  expect_identical(pars(model), pars(model, "all"))

  data <- bauw::peregrine
  data$Annual <- factor(data$Year)

  set.seed(34)

  # analyse
  analysis <- embr::analyse(model, data = data)

  expect_identical(class(analysis), c("smb_analysis", "mb_analysis"))
  expect_true(is.smb_analysis(analysis))

  expect_identical(universals::niters(analysis), 500L)
  expect_identical(universals::nchains(analysis), 2L)
  expect_identical(universals::nsims(analysis), 1000L)
  expect_identical(embr::ngens(analysis), 2000L)

  expect_identical(niters(analysis), 500L)
  expect_identical(ngens(analysis), 2000L)

  expect_identical(pars(analysis, "fixed"), pars(model, "fixed"))
  expect_identical(pars(analysis, "random"), pars(model, "random"))
  expect_identical(pars(analysis, "all"), pars(model, "all"))
  expect_identical(pars(analysis), pars(model))
  expect_identical(pars(analysis, "primary"), pars(model, "primary"))
  expect_identical(pars(analysis, "derived"), pars(model, "derived"))
  expect_identical(pars(analysis, "random"), "bAnnual")

  expect_is(as.mcmcr(analysis), "mcmcr")

  analysis <- reanalyse(analysis)
  expect_identical(universals::niters(analysis), 500L)
  expect_identical(universals::nchains(analysis), 2L)
  expect_identical(universals::nsims(analysis), 1000L)
  expect_identical(embr::ngens(analysis), 4000L)

  monitor <- rstan::monitor(analysis$stanfit, print = FALSE)
  rhat <- rhat(analysis, by = "term", as_df = TRUE)

  rhat_stan <- tibble::tibble(
    term = as.term(row.names(monitor)),
    rhat = round(monitor[, "Rhat"], 3)
  )
  rhat_stan <- rhat_stan[rhat_stan$term != "lp__", ]
  expect_identical(rhat$term, rhat_stan$term)

  glance <- glance(analysis)
  expect_is(glance, "tbl")
  expect_identical(glance$n, 40L)
  expect_identical(glance$K, 5L)
  expect_identical(glance$nthin, 2L)

  expect_identical(
    colnames(glance),
    c(
      "n", "K", "nchains", "niters", "nthin", "ess", "rhat",
      "converged"
    )
  )

  waic <- IC(analysis)
  expect_gt(waic, 305)
  expect_lt(waic, 315)

  coef <- coef(analysis, simplify = TRUE)

  expect_is(coef, "tbl")
  expect_is(coef, "mb_analysis_coef")
  expect_identical(
    colnames(coef),
    c("term", "estimate", "lower", "upper", "svalue")
  )
  expect_identical(
    coef$term,
    sort(as.term(c("alpha", "beta1", "beta2", "beta3", "log_sAnnual")))
  )
  expect_identical(
    coef(analysis, "derived", simplify = TRUE)$term,
    as.term("sAnnual")
  )
  expect_identical(
    coef(analysis, "all", simplify = TRUE)$term,
    sort(
      as.term(
        c(
          "alpha", paste0("bAnnual[", 1:40, "]"), "beta1", "beta2", "beta3",
          "log_sAnnual", "sAnnual"
        )
      )
    )
  )

  tidy <- tidy(analysis)
  expect_identical(
    colnames(tidy),
    c("term", "estimate", "lower", "upper", "esr", "rhat")
  )

  year <- predict(analysis, new_data = "Year")

  expect_is(year, "tbl")
  expect_identical(colnames(year), c(
    "Year", "Pairs", "R.Pairs", "Eyasses", "Annual",
    "estimate", "lower", "upper", "svalue"
  ))
  expect_true(all(year$estimate > year$lower))
  expect_true(all(year$estimate < year$upper))

  expect_equal(unlist(estimates(analysis)), coef$estimate, check.names = FALSE)
})
