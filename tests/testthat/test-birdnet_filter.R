test_that("data must be a tibble with required columns", {
  # Create a minimal valid tibble
  valid_data <- dplyr::tibble(
    filepath = "file1.csv",
    start = 1,
    end = 2,
    common_name = "Species A",
    scientific_name = "Species A",
    confidence = 0.9
  )

  # Should pass with valid data
  expect_silent(birdnet_filter(valid_data))

  # Not a tibble
  expect_silent(birdnet_filter(data = mtcars))

  # Missing required column
  invalid_data <- valid_data |>
    select(-common_name)
  expect_error(birdnet_filter(invalid_data, species = "Yellow-rumped Warbler"))
})


test_that("species must be NULL or a character vector without missing values", {
  valid_data <- dplyr::tibble(
    filepath = "file1.csv",
    start = 1,
    end = 2,
    common_name = "Species A",
    scientific_name = "Species A",
    confidence = 0.9
  )

  expect_silent(birdnet_filter(valid_data, species = NULL))
  expect_silent(birdnet_filter(valid_data, species = c("Species A", "Species B")))
  expect_error(birdnet_filter(valid_data, species = 123))
  expect_error(birdnet_filter(valid_data, species = c("A", NA)))
  expect_error(birdnet_filter(valid_data, species = character(0)))
})


test_that("threshold must be NULL, numeric vector between 0 and 1, or tibble with species and threshold columns", {
  valid_data <- dplyr::tibble(
    filepath = "file1.csv",
    start = 1,
    end = 2,
    common_name = "Species A",
    scientific_name = "Species A",
    confidence = 0.9
  )

  # numeric scalar/vector between 0 and 1
  expect_silent(birdnet_filter(valid_data, threshold = 0.5))
  expect_error(birdnet_filter(valid_data, threshold = c(0.1, 0.9)))
  expect_error(birdnet_filter(valid_data, threshold = -0.1))
  expect_error(birdnet_filter(valid_data, threshold = 1.5))
  expect_error(birdnet_filter(valid_data, threshold = NA_real_))

  # valid tibble with species and threshold columns
  valid_threshold_tbl <- dplyr::tibble(
    common_name = c("Species A", "Species B"),
    threshold = c(0.6, 0.8)
  )
  expect_silent(birdnet_filter(valid_data, threshold = valid_threshold_tbl))

  # missing required columns
  invalid_tbl <- dplyr::tibble(
    species = c("A", "B"),
    value = c(0.5, 0.7)
  )
  expect_error(birdnet_filter(valid_data, threshold = invalid_tbl), "must.include")

  # threshold column with invalid values
  invalid_values_tbl <- dplyr::tibble(
    common_name = c("A"),
    threshold = c(1.5)
  )
  expect_error(birdnet_filter(valid_data, threshold = invalid_values_tbl))
})



test_that("year must be NULL or numeric vector without missing", {
  valid_data <- dplyr::tibble(
    filepath = "file1.csv",
    start = 1,
    end = 2,
    common_name = "Species A",
    scientific_name = "Species A",
    confidence = 0.9
  )

  expect_silent(birdnet_filter(valid_data, year = NULL))
  expect_silent(birdnet_filter(valid_data, year = 2020))
  expect_silent(birdnet_filter(valid_data, year = c(2019, 2020)))
  expect_error(birdnet_filter(valid_data, year = NA_real_))
  expect_error(birdnet_filter(valid_data, year = "2020"))
})



test_that("min_date and max_date must be NULL or character strings in 'YYYY-MM-DD' format", {
  valid_data <- dplyr::tibble(
    filepath = "file1.csv",
    start = 1,
    end = 2,
    common_name = "Species A",
    scientific_name = "Species A",
    confidence = 0.9
  )

  expect_silent(birdnet_filter(valid_data, min_date = NULL, max_date = NULL))
  expect_silent(birdnet_filter(valid_data, min_date = "2023-01-01"))
  expect_silent(birdnet_filter(valid_data, max_date = "2023-12-31"))
  expect_error(birdnet_filter(valid_data, min_date = "01-01-2023"), "format")
  expect_error(birdnet_filter(valid_data, max_date = "2023/12/31"), "format")
  expect_error(birdnet_filter(valid_data, min_date = NA_character_))
})



test_that("hour must be NULL or numeric vector between 0 and 23", {
  valid_data <- dplyr::tibble(
    filepath = "file1.csv",
    start = 1,
    end = 2,
    common_name = "Species A",
    scientific_name = "Species A",
    confidence = 0.9
  )

  expect_silent(birdnet_filter(valid_data, hour = NULL))
  expect_silent(birdnet_filter(valid_data, hour = 0))
  expect_silent(birdnet_filter(valid_data, hour = c(0, 12, 23)))
  expect_error(birdnet_filter(valid_data, hour = -1))
  expect_error(birdnet_filter(valid_data, hour = 24))
  expect_error(birdnet_filter(valid_data, hour = c(12, NA)))
})

