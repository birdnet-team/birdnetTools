test_that("birdnet_detection_history returns correct structures and shapes", {
  # --- Setup Mock Data with realistic ARU filenames ---
  mock_data <- data.frame(
    filepath = c(
      "project/site-A/site-A_20260601_221000.wav",
      "project/site-A/site-A_20260602_053000.wav",
      "project/site-B/site-B_20260601_120000.wav"
    ),
    confidence = c(0.8, 0.9, 0.75),
    stringsAsFactors = FALSE
  )

  # Ensure the date column matches what birdnet_add_datetime() would extract
  mock_data$date <- as.Date(c("2026-06-01", "2026-06-02", "2026-06-01"))

  # Effort data covering both sites over a 3-day span
  mock_effort <- data.frame(
    site = rep(c("site-A", "site-B"), each = 3),
    date = rep(as.Date(c("2026-06-01", "2026-06-02", "2026-06-03")), 2),
    stringsAsFactors = FALSE
  )

  # --- Run Function ---
  res <- birdnet_detection_history(
    data = mock_data,
    effort_data = mock_effort,
    survey_interval = "1 day",
    min_unique_days = 1,
    i = -2
  )

  # --- Assertions ---
  expect_type(res, "list")
  expect_named(res, c("detection_history", "effort_matrix", "detection_summary"))
  expect_equal(dim(res$detection_history), c(2, 3))
  expect_equal(unname(res$detection_history["site-A", ]), c(1, 1, 0))
  expect_equal(unname(res$detection_history["site-B", ]), c(1, 0, 0))
})

test_that("birdnet_detection_history returns correct structures and shapes", {
  # --- Setup Mock Data ---
  # Detections spanning 2 distinct days for site-A, 1 day for site-B
  mock_data <- data.frame(
    filepath = c(
      "project/site-A/site-A_20260601_221000.wav",
      "project/site-A/site-A_20260602_053000.wav",
      "project/site-B/site-B_20260601_120000.wav"
    ),
    confidence = c(0.8, 0.9, 0.75),
    stringsAsFactors = FALSE
  )
  mock_data$date <- as.Date(c("2026-06-01", "2026-06-02", "2026-06-01"))

  # Effort data covering both sites over a 3-day span
  mock_effort <- data.frame(
    site = rep(c("site-A", "site-B"), each = 3),
    date = rep(as.Date(c("2026-06-01", "2026-06-02", "2026-06-03")), 2),
    stringsAsFactors = FALSE
  )

  # Mock internal column detection by returning expected mapping
  # (If birdnet_detect_columns is an exported internal helper, we ensure it maps nicely)

  # --- Run Function ---
  res <- birdnet_detection_history(
    data = mock_data,
    effort_data = mock_effort,
    survey_interval = "1 day",
    min_unique_days = 1
  )

  # --- Assertions ---
  # Check general structure
  expect_type(res, "list")
  expect_named(res, c("detection_history", "effort_matrix", "detection_summary"))

  # Check matrices
  expect_true(is.matrix(res$detection_history))
  expect_true(is.matrix(res$effort_matrix))
  expect_equal(dim(res$detection_history), c(2, 3)) # 2 sites, 3 occasions
  expect_equal(rownames(res$detection_history), c("site-A", "site-B"))

  # Check binary conversion and zero-filling
  # site-A has detections on day 1 and 2, effort but no detection on day 3
  expect_equal(unname(res$detection_history["site-A", ]), c(1, 1, 0))
  # site-B has detection on day 1, effort but no detection on days 2 and 3
  expect_equal(unname(res$detection_history["site-B", ]), c(1, 0, 0))

  # Check effort matrix default logic (no n_files)
  expect_equal(unname(res$effort_matrix["site-A", ]), c(1, 1, 1))
})





test_that("birdnet_detection_history filters based on min_unique_days", {
  mock_data <- data.frame(
    filepath = c(
      "project/site-A/site-A_20260601_221000.wav",
      "project/site-A/site-A_20260602_053000.wav",
      "project/site-B/site-B_20260601_120000.wav"
    ),
    confidence = c(0.8, 0.9, 0.75),
    stringsAsFactors = FALSE
  )
  mock_data$date <- as.Date(c("2026-06-01", "2026-06-02", "2026-06-01"))

  mock_effort <- data.frame(
    site = rep(c("site-A", "site-B"), each = 2),
    date = rep(as.Date(c("2026-06-01", "2026-06-02")), 2),
    stringsAsFactors = FALSE
  )

  # Require at least 2 unique detection days to keep a site
  res <- birdnet_detection_history(
    data = mock_data,
    effort_data = mock_effort,
    survey_interval = "1 day",
    min_unique_days = 2
  )

  # site-B only has 1 detection day, so it should be dropped completely from summaries,
  # resulting in 0s across all active effort sessions in the history matrix.
  expect_equal(unname(res$detection_history["site-B", ]), c(0, 0))
  expect_equal(unname(res$detection_history["site-A", ]), c(1, 1))
})

test_that("birdnet_detection_history handles continuous file-count effort tracking", {
  mock_data <- data.frame(
    filepath = c(
      "project/site-A/site-A_20260601_221000.wav",
      "project/site-A/site-A_20260602_053000.wav",
      "project/site-B/site-B_20260601_120000.wav"
    ),
    confidence = c(0.8, 0.9, 0.75),
    stringsAsFactors = FALSE
  )
  mock_data$date <- as.Date(c("2026-06-01", "2026-06-02", "2026-06-01"))

  # Effort contains file count variables
  mock_effort_files <- data.frame(
    site = c("site-A", "site-A"),
    date = as.Date(c("2026-06-01", "2026-06-02")),
    n_files = c(10, 12),
    stringsAsFactors = FALSE
  )

  res <- birdnet_detection_history(
    data = mock_data,
    effort_data = mock_effort_files,
    survey_interval = "1 day"
  )

  # Effort matrix should capture continuous quantitative file metrics instead of binary tags
  expect_equal(unname(res$effort_matrix["site-A", ]), c(10, 12))
})

test_that("birdnet_detection_history throws custom checkmate/rlang errors", {
  bad_data <- data.frame(wrong_col = c(1, 2, 3))
  good_effort <- data.frame(site = "site-A", date = as.Date("2026-06-01"))

  # Test invalid data input error
  expect_error(
    birdnet_detection_history(bad_data, good_effort, "1 day"),
    regexp = "missing required BirdNET columns"
  )

  # Test invalid interval string logic
  good_data <- data.frame(
    filepath = "path/site-A/f1.wav", confidence = 0.9,
    site = "site-A", date = as.Date("2026-06-01")
  )
  expect_error(
    birdnet_detection_history(good_data, good_effort, "bad_interval"),
    regexp = "must be a valid lubridate unit string"
  )

  # Test bounds parameters constraints
  expect_error(
    birdnet_detection_history(good_data, good_effort, "1 day", min_unique_days = 0),
    regexp = "Element 1 is not >= 1"
  )
})

