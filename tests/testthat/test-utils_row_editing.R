test_that("birdnet_filter_species() filters correctly", {
  df <- data.frame(
    common_name = c("Song Sparrow", "Swainson's Thrush", "Song Sparrow"),
    other_col = 1:3
  )

  out <- birdnet_filter_species(df, "Song Sparrow")
  expect_equal(nrow(out), 2)
  expect_true(all(out$common_name == "Song Sparrow"))
})

test_that("birdnet_filter_threshold() works for universal threshold", {
  df <- data.frame(
    scientific_name = c("Passerella iliaca", "Catharus ustulatus"),
    confidence = c(0.8, 0.6)
  )

  out <- birdnet_filter_threshold(df, threshold_arg = 0.7)
  expect_equal(nrow(out), 1)
  expect_equal(out$scientific_name, "Passerella iliaca")
})

test_that("birdnet_filter_threshold() works for species-specific thresholds", {
  df <- data.frame(
    scientific_name = c("Passerella iliaca", "Catharus ustulatus"),
    confidence = c(0.8, 0.6)
  )

  thresholds <- data.frame(
    scientific_name = c("Passerella iliaca", "Catharus ustulatus"),
    threshold = c(0.75, 0.7)
  )

  out <- birdnet_filter_threshold(df, thresholds)
  expect_equal(nrow(out), 1)
  expect_equal(out$scientific_name, "Passerella iliaca")
})

test_that("birdnet_filter_year() works with and without year column", {
  df <- data.frame(
    filepath = c("20230501 050000", "20240601 060000")
  )

  out <- birdnet_filter_year(df, year_arg = 2024)
  expect_equal(nrow(out), 1)
})

test_that("birdnet_filter_date_range() filters correctly", {
  df <- data.frame(
    filepath = c("20230501_050000", "20230601_060000")
  )

  out <- birdnet_filter_date_range(df, min_date = "2023-05-15", max_date = "2023-06-30")
  expect_equal(nrow(out), 1)
})

test_that("birdnet_filter_hour() filters by hour correctly", {
  df <- data.frame(
    filepath = c("20230501_050000", "20230601_060000")
  )

  out <- birdnet_filter_hour(df, hour_arg = 5)
  expect_equal(nrow(out), 1)
})
