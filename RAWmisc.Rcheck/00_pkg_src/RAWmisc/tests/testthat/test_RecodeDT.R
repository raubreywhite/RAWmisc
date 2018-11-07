context("RecodeDT")

test_that("recoding numeric", {
  data <- data.table::data.table("a"=1:4,"b"=11:14)
  switch <- c("1"="2")
  RAWmisc::RecodeDT(data,switch,"a")
  expect_equal(data,data.table("a"=c(2,2,3,4),"b"=11:14))
})

test_that("recoding character", {
  data <- data.table::data.table("a"=as.character(1:4),"b"=11:14)
  switch <- c("1"="2")
  RAWmisc::RecodeDT(data,switch,"a")
  expect_equal(data,data.table("a"=as.character(c(2,2,3,4)),"b"=11:14))
})
