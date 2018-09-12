context("Interactions ProcessStack")


test_that("simple interactions", {
  set.seed(4)
  x <- floor(1:10000/3500)
  interaction <- rep(c(0,1),5000)
  y <- 1*x+1*x*interaction+rnorm(10000)

  data <- data.frame(x=as.factor(x),y,interaction)
  assign("data", data, envir=globalenv())

  stack <- RAWmisc::CreateStackSkeleton(n=length(1))
  stack$regressionType <- "linear"
  stack$outcome <- "y"
  stack$exposure <- "x:interaction"
  stack$confounders <- list(c("x","interaction"))
  stack$data <- "data"
  stack$graphExposureScaleMultiply <- 2
  stack$graphExposureScaleAdd <- 5
  stack$graphReference <- 0
  stack$graphExposureLocationsLabels <- list(c(0.1,0.5,1))
  stack$graphExposureLocations <- list(c(0.1,0.5,1))
  stack$graphTitleMain <- "title"
  stack$graphTitleX <- "test"
  if(interactive()) stack$graphFileName <- "/git/test.png"

  a <- ProcessStack(stack=stack,i=1)

  expect_equal(round(a$a_b*10)/10,c(1,2))
})

test_that("interactions with spline", {
  set.seed(4)
  x <- floor(1:10000/3500)
  interaction <- rep(c(0,1),5000)
  y <- 1*x+1*x*interaction+rnorm(10000)

  data <- data.frame(x,y,interaction)
  assign("data", data, envir=globalenv())

  stack <- RAWmisc::CreateStackSkeleton(n=length(1))
  stack$regressionType <- "linear"
  stack$outcome <- "y"
  stack$exposure <- "splines::ns(x,df=2):interaction"
  stack$confounders <- list(c("splines::ns(x,df=2)","interaction"))
  stack$data <- "data"
  stack$graphExposureScaleMultiply <- 2
  stack$graphExposureScaleAdd <- 5
  stack$graphReference <- 0
  stack$graphExposureLocationsLabels <- list(c(0.1,0.5,1))
  stack$graphExposureLocations <- list(c(0.1,0.5,1))
  stack$graphTitleMain <- "title"
  stack$graphTitleX <- "test"


  a <- ProcessStack(stack=stack,i=1)

  expect_equal(a$a_b,NA)
})
