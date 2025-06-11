test_that("the function accepts an input for path", {
  expect_true(!is.null(birdnet_combine(path = test_path("data"))))
})
