test_that("birdnet_get_effort scans directories and aggregates file counts correctly", {
  # Create a temporary directory that self-destructs after this test block
  tmp_dir <- withr::local_tempdir()

  # Set up fake site directories
  site_a_dir <- file.path(tmp_dir, "Site-A")
  site_b_dir <- file.path(tmp_dir, "Site-B")
  dir.create(site_a_dir)
  dir.create(site_b_dir)

  # Create dummy audio files with realistic datetime stamps in the names
  # Site-A: 2 files on June 1st, 1 file on June 2nd
  file.create(file.path(site_a_dir, "Site-A_20260601_060000.wav"))
  file.create(file.path(site_a_dir, "Site-A_20260601_180000.wav"))
  file.create(file.path(site_a_dir, "Site-A_20260602_060000.WAV")) # test case-insensitivity

  # Site-B: 1 file on June 1st, 1 non-audio file (should be ignored)
  file.create(file.path(site_b_dir, "Site-B_20260601_120000.mp3"))
  file.create(file.path(site_b_dir, "summary_report.txt"))

  # --- Run the function ---
  # We use i = -2 because the immediate parent of the file will be "Site-A" or "Site-B"
  res <- birdnet_get_effort(path = tmp_dir, i = -2)

  # --- Assertions ---
  expect_s3_class(res, "data.frame")
  expect_named(res, c("site", "date", "n_files"))

  # Check that we have exactly 3 unique site-date effort combinations
  expect_equal(nrow(res), 3)

  # Verify specific aggregations
  site_a_efforts <- res[res$site == "Site-A", ]
  # Depending on how birdnet_add_datetime extracts it, we check the counts:
  # June 1st should have 2 files
  expect_equal(site_a_efforts$n_files[site_a_efforts$date == as.Date("2026-06-01")], 2)
  # June 2nd should have 1 file (even with uppercase .WAV)
  expect_equal(site_a_efforts$n_files[site_a_efforts$date == as.Date("2026-06-02")], 1)

  # Site-B should only count the .mp3, ignoring the .txt file
  site_b_efforts <- res[res$site == "Site-B", ]
  expect_equal(nrow(site_b_efforts), 1)
  expect_equal(site_b_efforts$n_files, 1)
})

test_that("birdnet_get_effort throws custom error if directory does not exist", {
  expect_error(
    birdnet_get_effort("this/path/does/not/exist/at/all"))
})

test_that("birdnet_get_effort handles an empty directory gracefully", {
  empty_dir <- withr::local_tempdir()

  res <- birdnet_get_effort(empty_dir)

  # It should return a 0-row data frame with the correct columns
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
  expect_named(res, c("site", "date", "n_files"))
})

