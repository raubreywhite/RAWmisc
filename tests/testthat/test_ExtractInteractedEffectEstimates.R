context("ExtractInteractedEffectEstimates")

test_that("interaction", {
  set.seed(4)
  x <- 1:100
  interaction <- rep(c(0,1),50)
  y <- x*2*interaction+rnorm(100)

  data <- data.frame(x,y,interaction)

  fit <- lm(y~x*interaction,data=data)
  res <- ExtractInteractedEffectEstimates(
    beta=coef(fit),
    va=vcov(fit),
    nameBase="x",
    nameInteractions="x:interaction")

  expect_equal(round(res$beta*10)/10,c(0,2))
})
