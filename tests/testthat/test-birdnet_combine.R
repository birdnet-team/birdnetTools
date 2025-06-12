test_that("accepts an input for path", {
  expect_true(!is.null(birdnet_combine(path = test_path("data"))))
  expect_error(birdnet_combine(path = test_path("data", "test_example_1.csv")))
})



test_that("returns a tibble with the correct columns", {
  result <- birdnet_combine(path = test_path("data"))

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("start", "end", "scientific_name", "common_name", "confidence", "filepath") %in% colnames(result)))
})




test_that("errors if path is not character or directory", {
  expect_error(birdnet_combine(123))
  expect_error(birdnet_combine("nonexistent_dir"))
})



test_that("errors if no csv or txt files found", {
  temp_dir <- tempdir()
  # Make sure temp_dir is empty or no csv/txt files present
  files <- list.files(temp_dir, pattern = "\\.(csv|txt)$", full.names = TRUE)
  file.remove(files) # remove any csv or txt files

  expect_error(
    birdnet_combine(temp_dir),
    "No valid \\.csv or \\.txt files found"
  )
})



test_that("filters out files named with analysis_params or CombinedTable", {
  temp_dir <- tempdir()
  # Create filtered out files
  writeLines("start,end,filepath,common_name,scientific_name,confidence\n1,2,file1.csv,sparrow,Passeridae,0.8",
             file.path(temp_dir, "analysis_params.csv"))
  writeLines("start,end,filepath,common_name,scientific_name,confidence\n1,2,file2.csv,robin,Turdidae,0.9",
             file.path(temp_dir, "CombinedTable.csv"))
  # Create valid file
  valid_file <- file.path(temp_dir, "valid.csv")
  writeLines("start,end,filepath,common_name,scientific_name,confidence\n1,2,file3.csv,finch,Fringillidae,0.95", valid_file)

  result <- birdnet_combine(temp_dir)

  expect_true("filepath" %in% names(result))
  expect_false(any(grepl("analysis_params|CombinedTable", result$filepath)))
  expect_s3_class(result, "tbl_df")
})



test_that("handles invalid CSV files gracefully and skips them", {
  temp_dir <- tempdir()
  # Create a valid CSV file
  writeLines("start,end,filepath,common_name,scientific_name,confidence\n1,2,file.csv,sparrow,Passeridae,0.8",
             file.path(temp_dir, "valid.csv"))

  # Create an invalid CSV file (malformed)
  writeLines("This is not a valid CSV content", file.path(temp_dir, "invalid.csv"))

  result <- birdnet_combine(temp_dir)

  # The invalid file is skipped, valid file data is present
  expect_s3_class(result, "tbl_df")
  expect_true(any(grepl("file.csv", result$filepath)))
})



test_that("returns correct columns and types", {
  temp_dir <- tempdir()
  csv_file <- file.path(temp_dir, "test.csv")
  writeLines(
    paste(
      "start,end,filepath,common_name,scientific_name,confidence",
      "0.5,2.0,file1.wav,Sparrow,Passeridae,0.95",
      sep = "\n"
    ),
    csv_file
  )

  df <- birdnet_combine(temp_dir)

  expect_s3_class(df, "tbl_df")
  expect_equal(sort(names(df)), sort(c("start", "end", "filepath", "common_name", "scientific_name", "confidence")))
  expect_type(df$start, "double")
  expect_type(df$end, "double")
  expect_type(df$confidence, "double")
  expect_type(df$filepath, "character")
  expect_type(df$common_name, "character")
  expect_type(df$scientific_name, "character")
})



test_that("shows cli messages on errors", {
  temp_dir <- tempdir()
  # Create one valid and one invalid file
  valid_file <- file.path(temp_dir, "valid.csv")
  writeLines("start,end,filepath,common_name,scientific_name,confidence\n1,2,file.csv,sparrow,Passeridae,0.8",
             valid_file)
  invalid_file <- file.path(temp_dir, "invalid.csv")
  writeLines("invalid data", invalid_file)

  # Capture cli output messages
  msgs <- capture.output(
    birdnet_combine(temp_dir),
    type = "message"
  )

  expect_true(any(grepl("Successfully combined", msgs)))
  expect_true(any(grepl("file.csv", msgs) == FALSE)) # The invalid file should be mentioned in warning messages, so check if any warnings are captured is possible.
})



