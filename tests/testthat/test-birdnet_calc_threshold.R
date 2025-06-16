test_that("Returns correct structure for precision method", {
  validated <- tibble::tibble(
    common_name = rep("Species A", 10),
    confidence = seq(0.1, 1, length.out = 10),
    validation = c(0, 0, 0, 0, 0, 1, 1, 1, 0, 1)
  )

  result <- birdnet_calc_threshold(
    validated_data = validated,
    precision = 0.9
  )

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("common_name", "threshold"))
  expect_equal(result$common_name, "Species A")
  expect_true(result$threshold >= 0 && result$threshold <= 1)
})




test_that("Returns correct structure for probability method", {
  validated <- tibble::tibble(
    common_name = rep("Species A", 10),
    confidence = seq(0.1, 1, length.out = 10),
    validation = c(0, 0, 0, 0, 0, 1, 1, 1, 0, 1)
  )

  result <- birdnet_calc_threshold(
    validated_data = validated,
    probability = 0.5
  )

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("common_name", "threshold"))
  expect_equal(result$common_name, "Species A")
  expect_true(result$threshold >= 0 && result$threshold <= 1)
})



test_that("Method is overridden by non-null precision/probability", {
  validated <- tibble::tibble(
    common_name = rep("Species A", 10),
    confidence = seq(0.1, 1, length.out = 10),
    validation = c(0, 0, 0, 0, 0, 1, 1, 1, 0, 1)
  )


  res <- birdnet_calc_threshold(
    validated_data = validated,
    precision = 0.9
  )


  expect_s3_class(res, "tbl_df")
})




test_that("Throws error when both precision and probability are given", {
  validated <- tibble::tibble(
    common_name = rep("Species D", 10),
    confidence = seq(0.1, 1, length.out = 10),
    validation = sample(0:1, 10, replace = TRUE)
  )

  expect_error(
    birdnet_calc_threshold(
      validated_data = validated,
      precision = 0.9,
      probability = 0.5
    ),
    "You must specify only one of"
  )
})



test_that("Throws error when validation values are invalid", {
  invalid_validated <- tibble::tibble(
    common_name = rep("Species E", 10),
    confidence = seq(0.1, 1, length.out = 10),
    validation = rep(2, 10)  # Invalid
  )

  expect_error(
    birdnet_calc_threshold(
      validated_data = invalid_validated,
      precision = 0.8
    ),
    "must only contain 0.*1"
  )
})



test_that("Throws error when required columns are missing", {
  bad_df <- tibble::tibble(
    species = rep("Species F", 10),
    confidence = runif(10),
    validation = sample(0:1, 10, replace = TRUE)
  )

  expect_error(
    birdnet_calc_threshold(
      validated_data = bad_df,
      precision = 0.9
    ),
    "common_name"
  )
})



test_that("Falls back to validated_data if full_data is NULL", {
  validated <- tibble::tibble(
    common_name = rep("Species A", 10),
    confidence = seq(0.1, 1, length.out = 10),
    validation = c(0, 0, 0, 0, 0, 1, 1, 1, 0, 1)
  )

  res <- birdnet_calc_threshold(
    validated_data = validated,
    precision = 0.8
  )

  expect_s3_class(res, "tbl_df")
})




test_that("Clamps thresholds outside observed range", {
  validated <- tibble::tibble(
    common_name = rep("Species A", 10),
    confidence = seq(0.9, 1, length.out = 10),
    validation = c(0, 0, 0, 1, 1, 1, 1, 1, 0, 1)
  )

  result <- birdnet_calc_threshold(
    validated_data = validated,
    probability = 0.1  # Should result in threshold < 0.9
  )

  expect_true(result$threshold >= 0.9 && result$threshold <= 1)
})
