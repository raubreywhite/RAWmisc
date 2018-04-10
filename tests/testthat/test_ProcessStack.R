context("ProcessStack")

test_that("simple spline", {
  set.seed(4)
  x <- 1:100
  interaction <- rep(c(0,1),50)
  y <- x*2+rnorm(100)

  data <- data.frame(x,y,interaction)
  assign("data", data, envir=globalenv())

  stack <- RAWmisc::CreateStackSkeleton(n=length(1))
  stack$regressionType <- "linear"
  stack$outcome <- "y"
  stack$exposure <- "splines::ns(x,df=2)"
  stack$confounders <- list(c("interaction"))
  stack$data <- "data"

  a <- RAWmisc::ProcessStack(stack=stack,i=1)

  expect_equal(round(a$c_b*10)/10,2)
})

test_that("simple spline", {
  set.seed(4)
  x <- 1:100
  interaction <- rep(c(0,1),50)
  y <- x*2+rnorm(100)

  data <- data.frame(x,y,interaction)
  assign("data", data, envir=globalenv())

  stack <- RAWmisc::CreateStackSkeleton(n=length(1))
  stack$regressionType <- "linear"
  stack$outcome <- "y"
  stack$exposure <- "splines::ns(x,df=2)"
  stack$confounders <- list(c("interaction"))
  stack$data <- "data"

  a <- RAWmisc::ProcessStack(stack=stack,i=1)

  stack$exposure <- "x"

  b <- RAWmisc::ProcessStack(stack=stack,i=1)

  expect_equal(round(a$c_b*10)/10,round(b$c_b*10)/10)
})

test_that("more complicated spline vs rms package", {
  library(rms)
  set.seed(4)
  x <- 1:100
  interaction <- rep(c(0,1),50)
  y <- x*2+rnorm(100)
  y[x>50] <- y[x>50]*2
  x <- x/100

  data <- data.frame(x,y,interaction)
  assign("data", data, envir=globalenv())

  fit0 <- lm(y~splines::ns(x,df=2),data=data)

  ddist0 <- datadist(data)
  ddist0$limits[["x"]][2] <- 0 ##### SETTING REFERENCE VALUE FOR NEUROTICISM
  ddist0$limits[["x"]][1] <- 0 ##### SETTING REFERENCE VALUE FOR NEUROTICISM
  ddist0$limits[["x"]][3] <- 1 ##### SETTING REFERENCE VALUE FOR NEUROTICISM
  assign("ddist0", ddist0, envir=globalenv())
  options(datadist='ddist0')
  fit <- Glm(y ~ rcs(x,3), data=data, family=gaussian(),x=T,y=T)
  summary(fit)

  stack <- RAWmisc::CreateStackSkeleton(n=length(1))
  stack$regressionType <- "linear"
  stack$outcome <- "y"
  stack$exposure <- "splines::ns(x,df=2)"
  stack$confounders <- list(c("interaction"))
  stack$data <- "data"

  a <- RAWmisc::ProcessStack(stack=stack,i=1)

  b <- as.data.frame(summary(fit))
  expect_equal(round(a$c_b),round(b$Effect))
})






