context("ExtractFits")

test_that("simple, linear", {
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

test_that("simple, logistic", {
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

test_that("simple, poisson", {
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


test_that("simple spline, linear", {

  set.seed(4)
  x <- 1:100
  interaction <- rep(c(0,1),50)
  y <- x*2+rnorm(100)

  data <- data.frame(x,y,interaction)
  fit0 <- lm(y~1,data=data)
  fit1 <- lm(y~splines::ns(x,df=4),data=data)

  stack <- RAWmisc::CreateStackSkeleton(n=1)
  stack$regressionType <- "linear"
  stack$outcome <- "y"
  stack$exposure <- "splines::ns(x,df=4)"
  stack$data <- "data"

  a <- ExtractFitsSplines(fit0=fit0,fit1=fit1,fit1aic=fit1, stack=stack, i=1, data=data, form="y~splines::ns(x,df=4)")
  expect_equal(round(a$b[1]*10)/10,2)
})

test_that("simple, linear with interaction", {
  set.seed(4)
  x <- 1:100
  interaction <- rep(c(0,1),50)
  y <- x*2+rnorm(100)

  data <- data.frame(x,y,interaction)
  fit0 <- lm(y~1,data=data)
  fit1 <- lm(y~x*interaction,data=data)

  a <- ExtractFits(fit0=fit0,fit1=fit1,fit1aic=fit1)
  expect_equal(round(a$b[2]*10)/10,2)
})

test_that("simple, linear with interaction", {
  set.seed(4)
  x <- 1:100
  interaction <- rep(c(0,1),50)
  y <- x*2+rnorm(100)

  data <- data.frame(x,y,interaction)
  fit0 <- lm(y~1,data=data)
  fit1 <- lm(y~x*interaction,data=data)

  a <- ExtractFits(fit0=fit0,fit1=fit1,fit1aic=fit1, nameBase="x", nameInteractions="x:interaction")
  expect_equal(round(a[exposure=="COMBINATION: x + x:interaction"]$b*10)/10,2)
})



