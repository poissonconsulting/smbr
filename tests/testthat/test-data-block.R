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

  mod_data <- nlist::as_nlist(modify_data(data, model))

  output <- data_block(mod_data)

  expect_equal(
    output,
    "data {\n  int X[nObs];\n  real Y[nObs];\n  int nObs;\n}"
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

  mod_data <- nlist::as_nlist(modify_data(data, model))

  output <- data_block(mod_data)

  expect_equal(
    output,
    "data {\n  int annual[nObs];\n  int site[nObs];\n  int quadrat[nObs];\n  int kelpline[nObs];\n  real temp[nObs];\n  int nsite;\n  int nObs;\n}"
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
    ),
    modify_data = function(data) {
      data$nObs <- NULL
      data
    }
  )

  mod_data <- nlist::as_nlist(modify_data(data, model))

  output <- data_block(mod_data)

  expect_equal(
    output,
    "data {\n  int annual[12];\n  int site[12];\n  int quadrat[12];\n  int kelpline[12];\n  real temp[12];\n  int nsite;\n}"
  )
})

test_that("create data block with a scalar real", {
  data <- data.frame(
    X = c(1L, 2L, 3L, 4L),
    Y = c(1.2, 7.3, 8.9, 2.6),
    Z = factor(c(1, 1, 2, 3))
  )

  model <- model("model {
    bY ~ dnorm(0, 2^-2)
    bX ~ dnorm(0, 2^-2)
    bZ ~ dnorm(0, 2^-2)
    sY ~ dnorm(0, 2^-2) T(0,)

    for (i in 1:nObs) {
      eY[i] <- bY + bX * X[i] + bZ * Z[i]
      Y[i] ~ dnorm(eY[i], sY^-2)
    }
  }",
    new_expr = "
    for(i in 1:nObs) {
      prediction[i] <- bY + bX * X[i] + bZ * Z[i]
      fit[i] <- prediction[i]
    }
  ",
    select_data = list(
      X = c(0L, 10L),
      Y = 1,
      Z = factor(1)
    ),
    modify_data = function(data) {
      data$nZ <- as.double(data$nZ)
      data
    }
  )

  mod_data <- nlist::as_nlist(modify_data(data, model))

  output <- data_block(mod_data)

  expect_equal(
    output,
    "data {\n  int X[nObs];\n  real Y[nObs];\n  int Z[nObs];\n  real nZ;\n  int nObs;\n}"
  )
})

test_that("create data block with a scalar real and no nObs", {
  data <- data.frame(
    X = c(1L, 2L, 3L, 4L),
    Y = c(1.2, 7.3, 8.9, 2.6),
    Z = factor(c(1, 1, 2, 3))
  )

  model <- model("model {
    bY ~ dnorm(0, 2^-2)
    bX ~ dnorm(0, 2^-2)
    bZ ~ dnorm(0, 2^-2)
    sY ~ dnorm(0, 2^-2) T(0,)

    for (i in 1:nObs) {
      eY[i] <- bY + bX * X[i] + bZ * Z[i]
      Y[i] ~ dnorm(eY[i], sY^-2)
    }
  }",
    new_expr = "
    for(i in 1:nObs) {
      prediction[i] <- bY + bX * X[i] + bZ * Z[i]
      fit[i] <- prediction[i]
    }
  ",
    select_data = list(
      X = c(0L, 10L),
      Y = 1,
      Z = factor(1)
    ),
    modify_data = function(data) {
      data$nZ <- as.double(data$nZ)
      data$nObs <- NULL
      data
    }
  )

  mod_data <- nlist::as_nlist(modify_data(data, model))

  output <- data_block(mod_data)

  expect_equal(
    output,
    "data {\n  int X[4];\n  real Y[4];\n  int Z[4];\n  real nZ;\n}"
  )
})

test_that("output empty data block with empty nlist passed", {
  expect_equal(
    data_block(nlist::nlist()),
    "data {\n}"
  )
})

test_that("create data block when zero length vectors in the list", {
  data <- nlist::nlist(
    X = integer(),
    Y = double(),
    Z = factor()
  )

  output <- data_block(data)

  expect_equal(
    output,
    "data {\n}"
  )
})

test_that("passes when one zero length integer vector in the list", {
  data <- nlist::nlist(
    X = integer()
  )

  output <- data_block(data)

  expect_equal(
    output,
    "data {\n}"
  )
})

test_that("passes when one zero length double vector in the list", {
  data <- nlist::nlist(
    X = double()
  )

  output <- data_block(data)

  expect_equal(
    output,
    "data {\n}"
  )
})

test_that("errors with no arguments passed", {
  expect_error(
    data_block(),
    regexp = 'argument "x" is missing, with no default'
  )
})

test_that("errors when NULL passed", {
  expect_error(
    data_block(NULL),
    regexp = "`x` must inherit from S3 class 'nlist'."
  )
})

test_that("errors when a number is passed as data argument", {
  expect_error(
    data_block(2),
    regexp = "`x` must inherit from S3 class 'nlist'."
  )
})

test_that("errors when a dataframe is passed", {
  data <- data.frame(
    X = c(1L, 2L, 3L, 4L),
    Y = c(1.2, 7.3, 8.9, 2.6),
    Z = factor(c(1, 1, 2, 3))
  )
  expect_error(
    data_block(data),
    regexp = "`x` must inherit from S3 class 'nlist'."
  )
})

test_that("errors when matrix passed in nlist", {
  data <- nlist::nlist(
    X = matrix(1:10, 2),
    Y = double(),
    Z = factor()
  )

  expect_error(
    data_block(data),
    regexp = "Matrix data type not currently implemented"
  )
})

test_that("errors when array passed in nlist", {
  data <- nlist::nlist(
    X = integer(),
    Y = array(1:10, 2),
    Z = factor()
  )

  expect_error(
    data_block(data),
    regexp = "Array data type not currently implemented"
  )
})
