# ------------------------------------------------------------------------------
test_that("parameters are validated", {
  testthat::expect_error( bluesky_download(dailyOutputDir = "DUMMY") )
  testthat::expect_error( bluesky_download(model = "DUMMY") )
  testthat::expect_error( bluesky_download(modelRun = NULL) )
  testthat::expect_error( bluesky_download(modelRun = 20190909) )
  testthat::expect_error( bluesky_download(subDir = "DUMMY") )
  testthat::expect_error( bluesky_download(baseUrl = "DUMMY") )
})

