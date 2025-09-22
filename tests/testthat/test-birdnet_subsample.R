
test_that("birdnet_subsample: input validation works", {

  valid_data <- readr::read_csv(testthat::test_path("data", "test_example_1.csv"))

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

  valid_data <- readr::read_csv(testthat::test_path("data", "test_example_1.csv"))
  result <- birdnet_subsample(valid_data, n = 20, method = "even_stratified")

  expect_s3_class(result, "data.frame")
})



test_that("birdnet_subsample: works with method = 'random'", {

  valid_data <- readr::read_csv(testthat::test_path("data", "test_example_1.csv"))
  result <- birdnet_subsample(valid_data, n = 20, method = "random")

  cols <- birdnet_detect_columns(result)

  expect_s3_class(result, "data.frame")
  expect_lte(all(result |>
               dplyr::summarize(count = dplyr::n(), .by = !!dplyr::sym(cols$scientific_name)) |>
               dplyr::pull(count)),
             20)  # max 5 per species
})

test_that("birdnet_subsample: works with method = 'top'", {

  valid_data <- readr::read_csv(testthat::test_path("data", "test_example_1.csv"))
  result <- birdnet_subsample(valid_data, n = 10, method = "top")

  cols <- birdnet_detect_columns(result)

  expect_s3_class(result, "data.frame")
  expect_lte(all(result |>
                   dplyr::summarize(count = dplyr::n(), .by = !!dplyr::sym(cols$scientific_name)) |>
                   dplyr::pull(count)), 10)
})



test_that("birdnet_subsample: file output is created if save_to_file = TRUE", {
  df <- dplyr::tibble(
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



test_that("birdnet_subsample: writes default file name when save_to_file = TRUE and file is NULL", {
  df <- dplyr::tibble(
    filepath = "audio.wav",
    start = 0,
    end = 3,
    common_name = rep("Robin", 5),
    scientific_name = rep("Turdus migratorius", 5),
    confidence = runif(5, 0.1, 1)
  )

  default_file <- file.path(tempdir(), "subsampled_data.csv")
  old_wd <- setwd(tempdir())
  on.exit(setwd(old_wd), add = TRUE)

  birdnet_subsample(df, n = 2, method = "random", save_to_file = TRUE)

  expect_true(file.exists(default_file))
  unlink(default_file)
})


test_that("birdnet_subsample: n larger than available rows per species returns all rows", {
  df <- dplyr::tibble(
    filepath = "audio.wav",
    start = 0,
    end = 3,
    common_name = c(rep("Robin", 3), rep("Sparrow", 2)),
    scientific_name = c(rep("Turdus migratorius", 3), rep("Passer domesticus", 2)),
    confidence = runif(5, 0.1, 1)
  )

  result <- birdnet_subsample(df, n = 10, method = "random")

  cols <- birdnet_detect_columns(result)
  counts <- result |>
    dplyr::count(!!rlang::sym(cols$common_name)) |>
    dplyr::pull(n)

  # should not exceed available per species
  expect_true(all(counts <= c(3, 2)))
})



test_that("birdnet_subsample: method is case-sensitive", {
  df <- dplyr::tibble(
    filepath = "audio.wav",
    start = 0,
    end = 3,
    common_name = rep("Robin", 5),
    scientific_name = rep("Turdus migratorius", 5),
    confidence = runif(5, 0.1, 1)
  )

  expect_error(birdnet_subsample(df, n = 3, method = "Top"), "one of")
})


test_that("birdnet_subsample: top method returns highest confidence rows", {
  df <- dplyr::tibble(
    filepath = "audio.wav",
    start = 0,
    end = 3,
    common_name = rep("Robin", 5),
    scientific_name = rep("Turdus migratorius", 5),
    confidence = c(0.2, 0.8, 0.5, 0.9, 0.1)
  )

  result <- birdnet_subsample(df, n = 2, method = "top")

  expect_true(all(result$confidence %in% c(0.9, 0.8)))
})



test_that("birdnet_subsample: stratified output does not contain category column", {
  df <- dplyr::tibble(
    filepath = "audio.wav",
    start = 0,
    end = 3,
    common_name = rep("Robin", 10),
    scientific_name = rep("Turdus migratorius", 10),
    confidence = runif(10, 0.1, 1)
  )

  result <- birdnet_subsample(df, n = 5, method = "stratified")
  expect_false("category" %in% names(result))
})


test_that("birdnet_subsample: accepts upper-case .CSV extension", {
  df <- dplyr::tibble(
    filepath = "audio.wav",
    start = 0,
    end = 3,
    common_name = rep("Robin", 5),
    scientific_name = rep("Turdus migratorius", 5),
    confidence = runif(5, 0.1, 1)
  )

  tmp <- tempfile(fileext = ".CSV")
  expect_silent(birdnet_subsample(df, n = 2, method = "random", save_to_file = TRUE, file = tmp))
  unlink(tmp)
})




test_that("birdnet_subsample: handles NA in confidence column for stratified method", {
  df <- dplyr::tibble(
    filepath = "audio.wav",
    start = 0,
    end = 3,
    common_name = rep("Robin", 10),
    scientific_name = rep("Turdus migratorius", 10),
    confidence = c(runif(8, 0.1, 1), NA, NA)
  )

  expect_error(birdnet_subsample(df, n = 5, method = "even_stratified"))

})
