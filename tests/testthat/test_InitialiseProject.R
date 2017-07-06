context("InitialiseProject")

test_that("recoding numeric", {
  AllowFileManipulationFromInitialiseProject()
  InitialiseProject <- InitialiseProject(HOME=tempdir(),
                                RAW=tempdir(),
                                CLEAN=tempdir(),
                                BAKED=tempdir(),
                                FINAL=tempdir(),
                                SHARED=tempdir())

  expect_equal("data","data")
})

