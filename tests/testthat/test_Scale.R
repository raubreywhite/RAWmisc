context("Scale")

test_that("creation of scale", {
  set.seed(4)
  d <- data.table::data.table("a"=rnorm(10000),"b"=rnorm(10000)*2+1)
  means <- apply(d,2,mean,na.rm=T)
  sds <- apply(d,2,sd,na.rm=T)

  expect_equal(as.numeric(round(ScaleCreate(d)$m)),c(0,1))
})

test_that("creation of scale", {
  set.seed(4)
  d <- data.table::data.table("a"=rnorm(10000),"b"=rnorm(10000)*2+1)
  means <- apply(d,2,mean,na.rm=T)
  sds <- apply(d,2,sd,na.rm=T)

  expect_equal(as.numeric(round(ScaleCreate(d)$sd)),c(1,2))
})

test_that("using scale, data.table", {
  set.seed(4)
  d1 <- data.table::data.table("a"=rnorm(10000),"b"=rnorm(10000)*2+1)
  d2 <- data.table::data.table("a"=rnorm(10000)+1,"b"=rnorm(10000)*2+1)

  s <- ScaleCreate(d1)

  expect_equal(as.numeric(round(apply(ScaleApply(d2,s),2,mean))),c(1,0))
})


test_that("using scale, data.frame", {
  set.seed(4)
  d1 <- data.frame("a"=rnorm(10000),"b"=rnorm(10000)*2+1)
  d2 <- data.frame("a"=rnorm(10000)+1,"b"=rnorm(10000)*2+1)

  s <- ScaleCreate(d1)

  expect_equal(as.numeric(round(apply(ScaleApply(d2,s),2,mean))),c(1,0))
})

test_that("using scale, matrix", {
  set.seed(4)
  d1 <- data.frame("a"=rnorm(10000),"b"=rnorm(10000)*2+1)
  d2 <- as.matrix(data.frame("a"=rnorm(10000)+1,"b"=rnorm(10000)*2+1))

  s <- ScaleCreate(d1)

  expect_equal(as.numeric(round(apply(ScaleApply(d2,s),2,mean))),c(1,0))
})
