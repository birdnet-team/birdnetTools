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

test_that("birdnet_add_site extracts the correct site from file paths", {
  # Setup dummy data with standard column names that birdnet_detect_columns would find
  # Assuming birdnet_detect_columns looks for 'filepath'
  mock_data <- data.frame(
    filepath = c("project/site-A/audio1.wav", "project/site-B/audio2.wav"),
    species = c("Cardinalis cardinalis", "Cyanocitta cristata"),
    stringsAsFactors = FALSE
  )

  # Test standard behavior (i = -2, immediate parent folder)
  res_default <- birdnet_add_site(mock_data, i = -2)
  expect_s3_class(res_default, "data.frame")
  expect_true("site" %in% colnames(res_default))
  expect_equal(res_default$site, c("site-A", "site-B"))

  # Test alternative index (i = -3, grandfather folder)
  res_parent <- birdnet_add_site(mock_data, i = -3)
  expect_equal(res_parent$site, c("project", "project"))
})

test_that("birdnet_add_site handles both forward and backward slashes", {
  # BirdNET users might be on Windows or Unix
  mixed_paths <- data.frame(
    filepath = c("windows\\style-site\\file.wav", "unix/style-site/file.wav"),
    stringsAsFactors = FALSE
  )

  res <- birdnet_add_site(mixed_paths, i = -2)
  expect_equal(res$site, c("style-site", "style-site"))
})

test_that("birdnet_add_site throws an error when filepath column is missing or undetectable", {
  # Data with completely unrelated columns
  bad_data <- data.frame(
    id = c(1, 2),
    confidence = c(0.8, 0.9)
  )

  # Expect an error containing our specific error message
  expect_error(
    birdnet_add_site(bad_data),
    regexp = "Could not automatically detect a valid file path column"
  )
})

test_that("birdnet_add_site handles NA values in filepath gracefully", {
  missing_path_data <- data.frame(
    filepath = c("project/site-A/audio1.wav", NA),
    stringsAsFactors = FALSE
  )

  res <- birdnet_add_site(missing_path_data, i = -2)
  expect_equal(res$site, c("site-A", NA_character_))
})


test_that("birdnet_add_site extracts the correct site from file paths", {
  # Setup dummy data with standard column names that birdnet_detect_columns would find
  # Assuming birdnet_detect_columns looks for 'filepath'
  mock_data <- data.frame(
    filepath = c("project/site-A/audio1.wav", "project/site-B/audio2.wav"),
    species = c("Cardinalis cardinalis", "Cyanocitta cristata"),
    stringsAsFactors = FALSE
  )

  # Test standard behavior (i = -2, immediate parent folder)
  res_default <- birdnet_add_site(mock_data, i = -2)
  expect_s3_class(res_default, "data.frame")
  expect_true("site" %in% colnames(res_default))
  expect_equal(res_default$site, c("site-A", "site-B"))

  # Test alternative index (i = -3, grandfather folder)
  res_parent <- birdnet_add_site(mock_data, i = -3)
  expect_equal(res_parent$site, c("project", "project"))
})

test_that("birdnet_add_site handles both forward and backward slashes", {
  # BirdNET users might be on Windows or Unix
  mixed_paths <- data.frame(
    filepath = c("windows\\style-site\\file.wav", "unix/style-site/file.wav"),
    stringsAsFactors = FALSE
  )

  res <- birdnet_add_site(mixed_paths, i = -2)
  expect_equal(res$site, c("style-site", "style-site"))
})

test_that("birdnet_add_site throws an error when filepath column is missing or undetectable", {
  # Data with completely unrelated columns
  bad_data <- data.frame(
    id = c(1, 2),
    confidence = c(0.8, 0.9)
  )

  # Expect an error containing our specific error message
  expect_error(
    birdnet_add_site(bad_data),
    regexp = "Could not automatically detect a valid file path column"
  )
})

test_that("birdnet_add_site handles NA values in filepath gracefully", {
  missing_path_data <- data.frame(
    filepath = c("project/site-A/audio1.wav", NA),
    stringsAsFactors = FALSE
  )

  res <- birdnet_add_site(missing_path_data, i = -2)
  expect_equal(res$site, c("site-A", NA_character_))
})



# helpers
make_df <- function(paths) data.frame(filepath = paths, stringsAsFactors = FALSE)
utc <- function(x) as.POSIXct(x, tz = "UTC")

# basics ------------------------------------------------------------------

test_that("birdnet_add_datetime adds all datetime columns with correct types", {
  res <- birdnet_add_datetime(make_df("some/path/20230401_123045.wav"), quiet = TRUE)

  expect_true(all(c("datetime", "date", "year", "month", "mday", "yday", "hour", "minute") %in% names(res)))
  expect_s3_class(res$datetime, "POSIXct")
  expect_s3_class(res$date, "Date")
  expect_equal(res$year, 2023)
  expect_equal(res$month, 4)
  expect_equal(res$mday, 1)
  expect_equal(res$yday, 91)
  expect_equal(res$hour, 12)
  expect_equal(res$minute, 30)
})

test_that("default formats parse the common year-first file names", {
  paths <- c(
    "a/00014530_20230518T133500.WAV",  # digit prefix must be ignored
    "a/20230518133500.WAV",
    "a/20230518_133500.WAV",
    "a/20230518_1335.WAV"
  )
  res <- birdnet_add_datetime(make_df(paths), quiet = TRUE)
  expect_equal(res$datetime, rep(utc("2023-05-18 13:35:00"), 4))
})

test_that("Windows-style paths work on any OS", {
  res <- birdnet_add_datetime(
    make_df("E:\\Audio\\2023_passerine\\14_18\\20230518_133500.WAV"),
    quiet = TRUE
  )
  expect_equal(res$datetime, utc("2023-05-18 13:35:00"))
})

test_that("NA file paths give NA datetimes without error", {
  res <- birdnet_add_datetime(make_df(c("a/20230518_133500.WAV", NA)), quiet = TRUE)
  expect_false(is.na(res$datetime[1]))
  expect_true(is.na(res$datetime[2]))
})

test_that("zero-row data frame doesn't error", {
  expect_error(res <- birdnet_add_datetime(make_df(character(0)), quiet = TRUE), NA)
  expect_equal(nrow(res), 0)
})

# time zone ---------------------------------------------------------------

test_that("tz sets the time zone and keeps the wall-clock time", {
  df <- make_df("a/20230401_123045.wav")
  res_utc <- birdnet_add_datetime(df, tz = "UTC", quiet = TRUE)
  res_van <- birdnet_add_datetime(df, tz = "America/Vancouver", quiet = TRUE)

  expect_equal(attr(res_van$datetime, "tzone"), "America/Vancouver")
  expect_equal(hour(res_van$datetime), 12)
  # 12:30 PDT (UTC-7 in April) is 7 hours later than 12:30 UTC
  expect_equal(as.numeric(difftime(res_van$datetime, res_utc$datetime, units = "hours")), 7)
})

# custom formats ----------------------------------------------------------

test_that("custom formats work, including literal separators", {
  df <- make_df(c("site1_2024-06-15_0530.wav", "site1_2024-06-16_0545.wav"))
  res <- birdnet_add_datetime(df, formats = "%Y-%m-%d_%H%M", quiet = TRUE)
  expect_equal(res$datetime, utc(c("2024-06-15 05:30:00", "2024-06-16 05:45:00")))
})

test_that("a hyphen between date and time works with a custom format", {
  res <- birdnet_add_datetime(
    make_df("some/path/20230502-124530.wav"),
    formats = "%Y%m%d-%H%M%S",
    quiet = TRUE
  )
  expect_equal(res$datetime, utc("2023-05-02 12:45:30"))
})

test_that("several custom formats can be mixed in one dataset", {
  df <- make_df(c("a/20230518_133500.WAV", "a/site1_2024-06-15_0530.wav"))
  res <- birdnet_add_datetime(df, formats = c("%Y%m%d_%H%M%S", "%Y-%m-%d_%H%M"), quiet = TRUE)
  expect_equal(res$datetime, utc(c("2023-05-18 13:35:00", "2024-06-15 05:30:00")))
})

# day-first / month-first --------------------------------------------------

test_that("unambiguous day-first and month-first names resolve correctly", {
  paths <- c("a/05182023_133500.WAV", "a/18052023_133500.WAV")
  msgs <- testthat::capture_messages(res <- birdnet_add_datetime(make_df(paths)))

  expect_equal(res$datetime, rep(utc("2023-05-18 13:35:00"), 2))
  expect_false(any(grepl("more than one format", msgs)))
})

test_that("names readable as both day-first and month-first are flagged", {
  df <- make_df(c("a/20230518_133500.WAV", "a/05062023_133500.WAV"))
  expect_message(res <- birdnet_add_datetime(df), "more than one format")

  expect_equal(res$datetime[1], utc("2023-05-18 13:35:00"))
  # the first listed format (day-first) wins
  expect_equal(res$datetime[2], utc("2023-06-05 13:35:00"))
})

test_that("the ambiguity message points to the affected rows", {
  df <- make_df(c("a/20230518_133500.WAV", "a/05062023_133500.WAV"))
  expect_message(birdnet_add_datetime(df), "Check row")
})

test_that("year-first names from 2001-2012 are not flagged as ambiguous", {
  msgs <- testthat::capture_messages(
    res <- birdnet_add_datetime(make_df("a/20100518_133500.WAV"))
  )
  expect_equal(res$datetime, utc("2010-05-18 13:35:00"))
  expect_false(any(grepl("more than one format", msgs)))
})

# messages ----------------------------------------------------------------

test_that("reports how many file names were parsed", {
  expect_message(
    birdnet_add_datetime(make_df("a/20230518_133500.WAV")),
    "Parsed datetime for 1 of 1"
  )
})

test_that("unparseable file names give NA and a message", {
  df <- make_df(c("a/20230518_133500.WAV", "a/not_a_date.WAV"))
  expect_message(res <- birdnet_add_datetime(df), "could not be parsed")

  expect_false(is.na(res$datetime[1]))
  expect_true(is.na(res$datetime[2]))
  expect_true(is.na(res$year[2]))
})

test_that("quiet = TRUE suppresses the status messages", {
  expect_silent(birdnet_add_datetime(make_df("a/20230518_133500.WAV"), quiet = TRUE))
})

# existing columns --------------------------------------------------------

test_that("returns data unchanged, with a warning, if all datetime columns exist", {
  once <- birdnet_add_datetime(make_df("a/20230518_133500.WAV"), quiet = TRUE)
  expect_message(twice <- birdnet_add_datetime(once), "already exist")
  expect_identical(twice, once)
})

test_that("warns when overwriting some existing datetime columns", {
  df <- make_df("a/20230518_133500.WAV")
  df$year <- 1999
  expect_message(res <- birdnet_add_datetime(df, quiet = TRUE), "overwritten")
  expect_equal(res$year, 2023)
})

# errors ------------------------------------------------------------------

test_that("birdnet_add_datetime errors clearly on bad input", {
  expect_error(birdnet_add_datetime(data.frame(x = 1)), "Can't find a column")
  expect_error(birdnet_add_datetime("not a data frame"))
  expect_error(birdnet_add_datetime(make_df("a"), formats = 1))
  expect_error(birdnet_add_datetime(make_df("a"), formats = character(0)))
  expect_error(birdnet_add_datetime(make_df("a"), tz = c("UTC", "UTC")))
})

