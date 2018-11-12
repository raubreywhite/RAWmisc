context("ProcessStack")


test_that("simple no spline", {
  set.seed(4)
  x <- 1:100
  interaction <- rep(c(0, 1), 50)
  y <- x * 2 + rnorm(100)

  data <- data.frame(x, y, interaction)
  assign("data", data, envir = globalenv())

  stack <- RAWmisc::CreateStackSkeleton(n = length(1))
  stack$regressionType <- "linear"
  stack$outcome <- "y"
  stack$exposure <- "x"
  stack$confounders <- list(c("interaction"))
  stack$data <- "data"
  stack$graphExposureScaleMultiply <- 2
  stack$graphExposureScaleAdd <- 5
  stack$graphReference <- 0
  stack$graphExposureLocationsLabels <- list(c(0.1, 0.5, 1))
  stack$graphExposureLocations <- list(c(0.1, 0.5, 1))
  stack$graphTitleMain <- "title"
  stack$graphTitleX <- "test"
  if (interactive()) stack$graphFileName <- "/git/test.png"

  a <- ProcessStack(stack = stack, i = 1)
  expect_equal(round(a$c_b * 10) / 10, 2)
})


test_that("simple spline", {
  set.seed(4)
  x <- 1:100
  interaction <- rep(c(0, 1), 50)
  y <- x * 2 + rnorm(100)

  data <- data.frame(x, y, interaction)
  assign("data", data, envir = globalenv())

  stack <- RAWmisc::CreateStackSkeleton(n = length(1))
  stack$regressionType <- "linear"
  stack$outcome <- "y"
  stack$exposure <- "splines::ns(x,df=2)"
  stack$confounders <- list(c("interaction"))
  stack$data <- "data"
  stack$graphExposureScaleMultiply <- 2
  stack$graphExposureScaleAdd <- 5
  stack$graphReference <- 0
  stack$graphExposureLocationsLabels <- list(c(0.1, 0.5, 1))
  stack$graphExposureLocations <- list(c(0.1, 0.5, 1))
  stack$graphTitleMain <- "title"
  stack$graphTitleX <- "test"
  if (interactive()) stack$graphFileName <- "/git/test.png"

  a <- ProcessStack(stack = stack, i = 1)

  expect_equal(round(a$c_b * 10) / 10, 2)
})

test_that("simple spline no labels", {
  set.seed(4)
  x <- 1:100
  interaction <- rep(c(0, 1), 50)
  y <- x * 2 + rnorm(100)

  data <- data.frame(x, y, interaction)
  assign("data", data, envir = globalenv())

  stack <- RAWmisc::CreateStackSkeleton(n = length(1))
  stack$regressionType <- "linear"
  stack$outcome <- "y"
  stack$exposure <- "splines::ns(x,df=2)"
  stack$confounders <- list(c("interaction"))
  stack$data <- "data"
  stack$graphExposureScaleMultiply <- 2
  stack$graphExposureScaleAdd <- 5
  stack$graphReference <- 0
  stack$graphExposureLocations <- list(c(0.1, 0.5, 1))
  stack$graphTitleMain <- "title"
  stack$graphTitleX <- "test"
  if (interactive()) stack$graphFileName <- "/git/test.png"

  a <- ProcessStack(stack = stack, i = 1)

  expect_equal(round(a$c_b * 10) / 10, 2)
})

test_that("simple spline", {
  set.seed(4)
  x <- 1:100
  interaction <- rep(c(0, 1), 50)
  y <- x * 2 + rnorm(100)

  data <- data.frame(x, y, interaction)
  assign("data", data, envir = globalenv())

  stack <- RAWmisc::CreateStackSkeleton(n = length(1))
  stack$regressionType <- "linear"
  stack$outcome <- "y"
  stack$exposure <- "splines::ns(x,df=2)"
  stack$confounders <- list(c("interaction"))
  stack$data <- "data"

  a <- RAWmisc::ProcessStack(stack = stack, i = 1)

  stack$exposure <- "x"

  b <- RAWmisc::ProcessStack(stack = stack, i = 1)

  expect_equal(round(a$c_b * 10) / 10, round(b$c_b * 10) / 10)
})

test_that("linear regression more complicated spline vs rms package, point estimate", {
  library(rms)
  set.seed(4)
  x <- 1:100
  interaction <- rep(c(0, 1), 50)
  y <- x * 2 + rnorm(100)
  y[x > 50] <- y[x > 50] * 2
  x <- x / 100

  data <- data.frame(x, y, interaction)
  assign("data", data, envir = globalenv())

  fit0 <- lm(y ~ splines::ns(x, df = 4), data = data)

  ddist0 <- datadist(data)
  ddist0$limits[["x"]][2] <- 0 ##### SETTING REFERENCE VALUE FOR NEUROTICISM
  ddist0$limits[["x"]][1] <- 0 ##### SETTING REFERENCE VALUE FOR NEUROTICISM
  ddist0$limits[["x"]][3] <- 1 ##### SETTING REFERENCE VALUE FOR NEUROTICISM
  assign("ddist0", ddist0, envir = globalenv())
  options(datadist = "ddist0")
  fit <- Glm(y ~ rcs(x, 3), data = data, family = gaussian(), x = T, y = T)
  summary(fit)

  stack <- RAWmisc::CreateStackSkeleton(n = length(1))
  stack$regressionType <- "linear"
  stack$outcome <- "y"
  stack$exposure <- "splines::ns(x,df=2)"
  stack$confounders <- list(c("interaction"))
  stack$data <- "data"

  a <- RAWmisc::ProcessStack(stack = stack, i = 1)

  b <- as.data.frame(summary(fit))
  expect_equal(
    c(round(a$c_b), round(a$c_se * 10) / 10),
    c(round(b$Effect), round(b$S.E. * 10) / 10)
  )
})

test_that("simple linear regression", {
  set.seed(4)
  x <- 1:100
  interaction <- rep(c(0, 1), 50)
  y <- x * 2 + rnorm(100)

  data <- data.frame(x, y, interaction)
  assign("data", data, envir = globalenv())

  stack <- RAWmisc::CreateStackSkeleton(n = length(1))
  stack$regressionType <- "linear"
  stack$outcome <- "y"
  stack$exposure <- "x"
  stack$confounders <- list(c("interaction"))
  stack$data <- "data"

  a <- RAWmisc::ProcessStack(stack = stack, i = 1)

  expect_equal(round(a$c_b * 10) / 10, 2)
})

test_that("simple logistic regression", {
  set.seed(4)
  x <- rep(1:1000, 20)
  interaction <- rep(c(0, 1), 50)
  z <- -7 + 0.015 * x
  pr <- 1 / (1 + exp(-z)) # pass through an inv-logit function
  y <- runif(length(pr)) < pr

  data <- data.frame(x, y, interaction)
  assign("data", data, envir = globalenv())

  stack <- RAWmisc::CreateStackSkeleton(n = length(1))
  stack$regressionType <- "logistic"
  stack$outcome <- "y"
  stack$exposure <- "x"
  stack$confounders <- list(c("interaction"))
  stack$data <- "data"

  a <- RAWmisc::ProcessStack(stack = stack, i = 1)

  expect_equal(round(a$c_b * 1000) / 1000, 0.015)
})

test_that("simple poisson regression", {
  set.seed(4)
  x <- rep(1:1000, 20)
  interaction <- rep(c(0, 1), 50)
  z <- exp(-7 + 0.015 * x)
  y <- rpois(length(z), z)

  data <- data.frame(x, y, interaction)
  assign("data", data, envir = globalenv())

  stack <- RAWmisc::CreateStackSkeleton(n = length(1))
  stack$regressionType <- "poisson"
  stack$outcome <- "y"
  stack$exposure <- "x"
  stack$confounders <- list(c("interaction"))
  stack$data <- "data"

  a <- RAWmisc::ProcessStack(stack = stack, i = 1)

  expect_equal(round(a$c_b * 1000) / 1000, 0.015)
})

test_that("simple negative binomial regression", {
  set.seed(4)
  x <- rep(1:1000, 1)
  interaction <- rep(c(0, 1), 50)
  z <- exp(-7 + 0.015 * x)
  y <- rpois(length(z), z)

  data <- data.frame(x, y, interaction)
  assign("data", data, envir = globalenv())

  stack <- RAWmisc::CreateStackSkeleton(n = length(1))
  stack$regressionType <- "negbin"
  stack$outcome <- "y"
  stack$exposure <- "x"
  stack$confounders <- list(c("interaction"))
  stack$data <- "data"

  a <- RAWmisc::ProcessStack(stack = stack, i = 1)

  expect_equal(round(a$c_b * 1000) / 1000, 0.015)
})

test_that("logistic regression more complicated spline vs rms package", {
  library(rms)
  set.seed(4)
  x <- c(400:599) / 1000
  interaction <- rep(c(0, 1), 50)
  z <- -1 + 1.5 * x
  pr <- 1 / (1 + exp(-z)) # pass through an inv-logit function
  y <- runif(length(pr)) < pr

  data <- data.frame(x, y, interaction)
  assign("data", data, envir = globalenv())

  fit0 <- glm(y ~ splines::ns(x, df = 4), data = data, family = binomial())

  ddist0 <- datadist(data)
  ddist0$limits[["x"]][2] <- 0 ##### SETTING REFERENCE VALUE FOR NEUROTICISM
  ddist0$limits[["x"]][1] <- 0 ##### SETTING REFERENCE VALUE FOR NEUROTICISM
  ddist0$limits[["x"]][3] <- 1 ##### SETTING REFERENCE VALUE FOR NEUROTICISM
  assign("ddist0", ddist0, envir = globalenv())
  options(datadist = "ddist0")
  fit <- Glm(y ~ rcs(x, 3), data = data, family = binomial(), x = T, y = T)
  summary(fit)

  stack <- RAWmisc::CreateStackSkeleton(n = length(1))
  stack$regressionType <- "logistic"
  stack$outcome <- "y"
  stack$exposure <- "splines::ns(x,df=2)"
  stack$confounders <- list(c("interaction"))
  stack$data <- "data"

  a <- RAWmisc::ProcessStack(stack = stack, i = 1)

  b <- as.data.frame(summary(fit))
  expect_equal(round(a$c_b * 10) / 10, round(b$Effect * 10) / 10)
})


test_that("expand stack", {
  set.seed(4)
  x1 <- 1:100
  x2 <- 100:1
  x3 <- runif(100)
  interaction <- rep(c(0, 1), 50)
  y <- x1 * 1 + x2 * 2 + rnorm(100)

  data <- data.frame(x1, x2, x3, y, interaction)
  assign("data", data, envir = globalenv())

  stack <- RAWmisc::CreateStackSkeleton(n = length(1))
  stack$regressionType <- "linear"
  stack$outcome <- "y"
  stack$exposure <- "x1"
  stack$confounders <- list(c("x2", "x3"))
  stack$data <- "data"
  stack$graphExposureScaleMultiply <- 2
  stack$graphExposureScaleAdd <- 5
  stack$graphReference <- 0
  stack$graphExposureLocationsLabels <- list(c(0.1, 0.5, 1))
  stack$graphExposureLocations <- list(c(0.1, 0.5, 1))
  stack$graphTitleMain <- "title"
  stack$graphTitleX <- "test"
  if (interactive()) stack$graphFileName <- "/git/test.png"

  a <- ExpandStack(stack = stack)

  expect_equal(a$exposure, c("x1", "x2", "x3"))
})



test_that("expand stack x2", {
  set.seed(4)
  x1 <- 1:100
  x2 <- 100:1
  x3 <- runif(100)
  interaction <- rep(c(0, 1), 50)
  y <- x1 * 1 + x2 * 2 + rnorm(100)

  data <- data.frame(x1, x2, x3, y, interaction)
  assign("data", data, envir = globalenv())

  stack <- RAWmisc::CreateStackSkeleton(n = 2)
  stack$regressionType <- "linear"
  stack$outcome <- "y"
  stack$exposure <- "x1"
  stack$confounders <- list(c("x2", "x3"))
  stack$data <- "data"
  stack$graphExposureScaleMultiply <- 2
  stack$graphExposureScaleAdd <- 5
  stack$graphReference <- 0
  stack$graphExposureLocationsLabels <- list(c(0.1, 0.5, 1))
  stack$graphExposureLocations <- list(c(0.1, 0.5, 1))
  stack$graphTitleMain <- "title"
  stack$graphTitleX <- "test"
  if (interactive()) stack$graphFileName <- "/git/test.png"

  a <- ExpandStack(stack = stack)

  expect_equal(a$exposure, c("x1", "x2", "x3", "x1", "x2", "x3"))
})

test_that("expand stack and process", {
  set.seed(4)
  x1 <- 1:100
  x2 <- 100:1
  x3 <- runif(100)
  interaction <- rep(c(0, 1), 50)
  y <- x1 * 1 + x2 * 2 + rnorm(100)

  data <- data.frame(x1, x2, x3, y, interaction)
  assign("data", data, envir = globalenv())

  stack <- RAWmisc::CreateStackSkeleton(n = length(1))
  stack$regressionType <- "linear"
  stack$outcome <- "y"
  stack$exposure <- "x1"
  stack$confounders <- list(c("x2", "x3"))
  stack$data <- "data"
  stack$graphExposureScaleMultiply <- 2
  stack$graphExposureScaleAdd <- 5
  stack$graphReference <- 0
  stack$graphExposureLocationsLabels <- list(c(0.1, 0.5, 1))
  stack$graphExposureLocations <- list(c(0.1, 0.5, 1))
  stack$graphTitleMain <- "title"
  stack$graphTitleX <- "test"
  if (interactive()) stack$graphFileName <- "/git/test.png"

  stackExpanded <- ExpandStack(stack = stack)
  a <- RAWmisc::ProcessStack(stack = stackExpanded, i = 1)
  b <- RAWmisc::ProcessStack(stack = stack, i = 1)

  expect_equal(a, b)
})


test_that("validate no exposure in confounders", {
  set.seed(4)
  x <- 1:100
  interaction <- rep(c(0, 1), 50)
  y <- x * 2 + rnorm(100)

  data <- data.frame(x, y, interaction)
  assign("data", data, envir = globalenv())

  stack <- RAWmisc::CreateStackSkeleton(n = length(1))
  stack$regressionType <- "linear"
  stack$outcome <- "y"
  stack$exposure <- "x"
  stack$confounders <- list(c("x"))
  stack$data <- "data"
  stack$graphExposureScaleMultiply <- 2
  stack$graphExposureScaleAdd <- 5
  stack$graphReference <- 0
  stack$graphExposureLocationsLabels <- list(c(0.1, 0.5, 1))
  stack$graphExposureLocations <- list(c(0.1, 0.5, 1))
  stack$graphTitleMain <- "title"
  stack$graphTitleX <- "test"
  if (interactive()) stack$graphFileName <- "/git/test.png"

  testthat::expect_error(ProcessStack(stack = stack, i = 1))
})
