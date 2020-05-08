library(testthat2020)

test_that("test 1", {
  #Sys.sleep(10)
  expect_equal(1, 1)
  expect_equal(1, 2)
  stop("hi")
})
