context("ExtractFits")

test_that("simple spline, linear", {
  set.seed(4)
  x <- 1:100
  interaction <- rep(c(0,1),50)
  y <- x*2+rnorm(100)

  data <- data.frame(x,y,interaction)
  fit0 <- lm(y~1,data=data)
  fit1 <- lm(y~x,data=data)

  a <- ExtractFits(fit0=fit0,fit1=fit1,fit1aic=fit1)
  expect_equal(round(a$b[2]*10)/10,2)
})

test_that("simple spline, logistic", {
  set.seed(4)
  x <- 1:100
  interaction <- rep(c(0,1),50)
  y <- x*2
  y <- y/max(y)
  y <- ifelse(runif(length(y))<y,1,0)

  data <- data.frame(x,y,interaction)
  fit0 <- glm(y~1,data=data,family=binomial())
  fit1 <- glm(y~x,data=data,family=binomial())

  a <- ExtractFits(fit0=fit0,fit1=fit1,fit1aic=fit1)
  expect_equal(round(a$b[2]*100)/100,0.05)
})

test_that("simple spline, poisson", {
  set.seed(4)
  x <- 1:100
  interaction <- rep(c(0,1),50)
  y <- round(x*2+rnorm(100))

  data <- data.frame(x,y,interaction)
  fit0 <- glm(y~1,data=data,family=poisson())
  fit1 <- glm(y~x,data=data,family=poisson())

  a <- ExtractFits(fit0=fit0,fit1=fit1,fit1aic=fit1)
  expect_equal(round(a$b[2]*100)/100,0.02)
})






