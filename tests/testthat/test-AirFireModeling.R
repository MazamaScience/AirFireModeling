# ------------------------------------------------------------------------------
testthat::context("Environment dataDir")

testthat::test_that("setModelDataDir and getModelDataDir work correctly", {
  
  skip_on_cran()
  skip_on_travis()
  
  # Setup
  modelDataDir <- try(getModelDataDir(), silent = TRUE)
  
  setModelDataDir("~")
  testthat::expect_equal(path.expand("~"), getModelDataDir())
  setModelDataDir(getwd())
  testthat::expect_equal(getwd(), getModelDataDir())
  
  # Teardown
  if (class(modelDataDir) == "character") {
    setModelDataDir(modelDataDir)
  } else {
    removeModelDataDir()
  }
  
})

