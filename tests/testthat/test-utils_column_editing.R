library(dplyr)
library(lubridate)

test_that("birdnet_add_datetime adds correct datetime columns", {
  # Create a minimal test df with a filepath column containing a datetime string
  df <- data.frame(
    filepath = c("some/path/20230401_123045.wav", "other/path/20230502-124530.wav"),
    stringsAsFactors = FALSE
  )

  result <- birdnet_add_datetime(df)

  expect_true(all(c("datetime", "date", "year", "month", "mday", "yday", "hour", "minute") %in% colnames(result)))

  # Check the datetime column is POSIXct
  expect_s3_class(result$datetime, "POSIXct")

  # Check correct parsing of the first datetime
  expect_equal(year(result$datetime[1]), 2023)
  expect_equal(month(result$datetime[1]), 4)
  expect_equal(day(result$datetime[1]), 1)
  expect_equal(hour(result$datetime[1]), 12)
  expect_equal(minute(result$datetime[1]), 30)
})

test_that("birdnet_add_datetime works with different timezone", {
  df <- data.frame(filepath = "some/path/20230401_123045.wav", stringsAsFactors = FALSE)
  result_utc <- birdnet_add_datetime(df, tz = "UTC")
  result_est <- birdnet_add_datetime(df, tz = "America/New_York")

  expect_false(identical(result_utc$datetime, result_est$datetime))
})

test_that("birdnet_drop_datetime removes datetime columns if present", {
  df <- data.frame(
    a = 1:3,
    datetime = Sys.time() + 1:3,
    year = 2020,
    month = 1:3,
    other = 5:7
  )
  result <- birdnet_drop_datetime(df)

  expect_false(any(c("datetime", "year", "month") %in% colnames(result)))
  expect_true(all(c("a", "other") %in% colnames(result)))
})

test_that("birdnet_drop_datetime returns data unchanged if no datetime columns", {
  df <- data.frame(x = 1:3, y = 4:6)
  result <- birdnet_drop_datetime(df)
  expect_identical(df, result)
})

test_that("birdnet_clean_names standardizes column names", {
  df <- data.frame(
    Start.Time = 1,
    End.Time = 2,
    Scientific = "sp1",
    Common = "common1",
    File.Name = "file1.wav",
    Confidence.Score = 0.95,
    stringsAsFactors = FALSE
  )
  result <- birdnet_clean_names(df)
  expect_named(result, c("start", "end", "scientific_name", "common_name", "filepath", "confidence"))
})

test_that("birdnet_clean_names leaves columns unchanged if no matches", {
  df <- data.frame(a = 1, b = 2)
  result <- birdnet_clean_names(df)
  expect_named(result, c("a", "b"))
})

test_that("birdnet_detect_columns identifies correct columns or returns NA", {
  df <- data.frame(
    StartTime = 1,
    EndTime = 2,
    ScientificName = "sp1",
    CommonName = "common1",
    ConfidenceScore = 0.9,
    FilePath = "file.wav",
    stringsAsFactors = FALSE
  )
  detected <- birdnet_detect_columns(df)

  expect_equal(detected$start, "StartTime")
  expect_equal(detected$end, "EndTime")
  expect_equal(detected$scientific_name, "ScientificName")
  expect_equal(detected$common_name, "CommonName")
  expect_equal(detected$confidence, "ConfidenceScore")
  expect_equal(detected$filepath, "FilePath")

  # Test with missing columns
  df2 <- data.frame(x = 1)
  detected2 <- birdnet_detect_columns(df2)
  expect_true(all(vapply(detected2, function(x) is.na(x), logical(1))))
})

