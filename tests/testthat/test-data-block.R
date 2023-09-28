test_that("create simple data block with X as integer and Y as double", {

  data <- data.frame(
    X = c(1L, 2L, 3L, 4L),
    Y = c(1.2, 7.3, 8.9, 2.6)
  )

  model <- model("model {
    bY ~ dnorm(0, 2^-2)
    bX ~ dnorm(0, 2^-2)
    sY ~ dnorm(0, 2^-2) T(0,)

    for (i in 1:nObs) {
      eY[i] <- bY + bX * X[i]
      Y[i] ~ dnorm(eY[i], sY^-2)
    }
  }",
  new_expr = "
    for(i in 1:nObs) {
      prediction[i] <- bY + bX * X[i]
      fit[i] <- prediction[i]
    }
  ",
  select_data = list(
    X = c(0L, 10L),
    Y = 1
    )
  )

  mod_data <- modify_data(data, model)

  output <- data_block(mod_data)

  expect_equal(
    output,
    "data {int X[nObs];real Y[nObs];int nObs;}"
  )
})

test_that("create data block with integer, double, logical and factor present in data set", {

  data <- data.frame(
    annual = c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L),
    site = factor(c(1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L)),
    quadrat = c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L),
    kelpline = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    temp = c(10.2, -0.3, 12.7, 16.4, 10.3, 12.6, 14.3, 13.2, 17.3, 13.4, 12.5, 14.5)
  )

  model <- model("model {
    bannual ~ dnorm(0, 2^-2)
    bsite ~ dnorm(0, 2^-2)
    bquadrat ~ dnorm(0, 2^-2)
    bkelpline ~ dnorm(0, 2^-2)
    btemp ~ dnorm(0, 2^-2)
    stemp ~ dnorm(0, 2^-2) T(0,)


    for (i in 1:nObs) {
      etemp[i] <- btemp + bannual * annual[i] + bsite * site[i] + bquadrat * quadrat[i] + bkelpline * kelpline[i]
      temp[i] ~ dnorm(etemp[i], stemp^-2)
    }
  }",
  new_expr = "
    for(i in 1:nObs) {
      prediction[i] <- btemp + bannual * annual[i] + bsite * site[i] + bquadrat * quadrat[i] + bkelpline * kelpline[i]
      fit[i] <- prediction[i]
    }
  ",
  select_data = list(
    annual = c(0L, 10L),
    site = factor(1),
    quadrat = c(1L, 3L),
    kelpline = logical(1),
    temp = c(-5, 20)
  )
  )

  mod_data <- modify_data(data, model)

  output <- data_block(mod_data)

  expect_equal(
    output,
    "data {int annual[nObs];int site[nObs];int quadrat[nObs];int kelpline[nObs];real temp[nObs];int nsite;int nObs;}"
  )
})

test_that("create data block with no nObs", {

  data <- data.frame(
    annual = c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L),
    site = factor(c(1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L)),
    quadrat = c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L),
    kelpline = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    temp = c(10.2, -0.3, 12.7, 16.4, 10.3, 12.6, 14.3, 13.2, 17.3, 13.4, 12.5, 14.5)
  )

  model <- model("model {
    bannual ~ dnorm(0, 2^-2)
    bsite ~ dnorm(0, 2^-2)
    bquadrat ~ dnorm(0, 2^-2)
    bkelpline ~ dnorm(0, 2^-2)
    btemp ~ dnorm(0, 2^-2)
    stemp ~ dnorm(0, 2^-2) T(0,)


    for (i in 1:nObs) {
      etemp[i] <- btemp + bannual * annual[i] + bsite * site[i] + bquadrat * quadrat[i] + bkelpline * kelpline[i]
      temp[i] ~ dnorm(etemp[i], stemp^-2)
    }
  }",
  new_expr = "
    for(i in 1:nObs) {
      prediction[i] <- btemp + bannual * annual[i] + bsite * site[i] + bquadrat * quadrat[i] + bkelpline * kelpline[i]
      fit[i] <- prediction[i]
    }
  ",
  select_data = list(
    annual = c(0L, 10L),
    site = factor(1),
    quadrat = c(1L, 3L),
    kelpline = logical(1),
    temp = c(-5, 20)
  )
  )

  mod_data <- modify_data(data, model)
  mod_data$nObs <- NULL

  output <- data_block(mod_data)

  expect_equal(
    output,
    "data {int annual[12];int site[12];int quadrat[12];int kelpline[12];real temp[12];int nsite;}"
  )
})
