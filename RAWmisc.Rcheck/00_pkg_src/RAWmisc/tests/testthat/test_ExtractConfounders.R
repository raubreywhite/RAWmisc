context("ExtractExposureConfounders")

test_that("splines", {
  res <- ExtractExposureConfounders("splines::ns(x,df=2)")
  expect_equal(res, "x")
})

test_that("interaction", {
  res <- ExtractExposureConfounders("a*b")
  expect_equal(res, c("a", "b"))
})
