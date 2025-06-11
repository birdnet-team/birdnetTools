
test_that("birdnet_subsample: input validation works", {

  valid_data <- read_csv(testthat::test_path("data", "test_example_1.csv"))

  # Should error if data is not a dataframe
  testthat::expect_error(birdnet_subsample("not a df", n = 10), "character")

  # Should error if n is not a positive integer
  testthat::expect_error(birdnet_subsample(valid_data, n = -5))
  testthat::expect_error(birdnet_subsample(valid_data, n = 1.5))

  # Should error if method is invalid
  expect_error(birdnet_subsample(valid_data, n = 5, method = "badmethod"), "one of")

  # Should error if save_to_file is not logical
  expect_error(birdnet_subsample(valid_data, n = 5, save_to_file = "yes"), "logical")

  # Should error if file is not .csv
  expect_error(birdnet_subsample(valid_data, n = 5, file = "not_csv.txt"), "end with '.csv'")
})



test_that("birdnet_subsample: works with method = 'stratified'", {

  valid_data <- read_csv(testthat::test_path("data", "test_example_1.csv"))
  result <- birdnet_subsample(valid_data, n = 20, method = "stratified")

  expect_s3_class(result, "data.frame")
  expect_true(all(c("common_name", "confidence") %in% colnames(result)))
})



test_that("birdnet_subsample: works with method = 'random'", {

  valid_data <- read_csv(testthat::test_path("data", "test_example_1.csv"))
  result <- birdnet_subsample(valid_data, n = 20, method = "random")

  expect_s3_class(result, "data.frame")
  expect_lte(all(result |>
               dplyr::summarize(count = dplyr::n(), .by = scientific_name) |>
               dplyr::pull(count)),
             20)  # max 5 per species
})

test_that("birdnet_subsample: works with method = 'top'", {

  valid_data <- read_csv(testthat::test_path("data", "test_example_1.csv"))
  result <- birdnet_subsample(valid_data, n = 10, method = "top")

  expect_s3_class(result, "data.frame")
  expect_lte(all(result |>
                   dplyr::summarize(count = dplyr::n(), .by = scientific_name) |>
                   dplyr::pull(count)), 10)
})



test_that("birdnet_subsample: file output is created if save_to_file = TRUE", {
  df <- tibble::tibble(
    filepath = "audio.wav",
    start = 0,
    end = 3,
    common_name = rep("Robin", 10),
    scientific_name = rep("Turdus migratorius", 10),
    confidence = runif(10, 0.1, 1)
  )

  tmp <- tempfile(fileext = ".csv")
  birdnet_subsample(df, n = 5, method = "random", save_to_file = TRUE, file = tmp)
  expect_true(file.exists(tmp))
  unlink(tmp)
})
