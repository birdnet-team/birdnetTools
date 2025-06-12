test_that("birdnet_heatmap argument checks work", {

  valid_data <- read_csv(test_path("data", "test_example_1.csv"))

  # 1. data must be tibble with required columns
  expect_error(birdnet_heatmap(data = mtcars))
  expect_silent(birdnet_heatmap(data = valid_data))

  # 2. species: must be NULL or character vector with no missing
  expect_error(birdnet_heatmap(data = valid_data, species = 123))
  expect_error(birdnet_heatmap(data = valid_data, species = c("Robin", NA)), "missing")
  expect_silent(birdnet_heatmap(data = valid_data, species = "American Robin"))
  expect_silent(birdnet_heatmap(data = valid_data, species = c("Robin", "Sparrow")))

  # 3. threshold: NULL or numeric scalar/vector [0,1] OR data.frame with cols scientific_name, threshold
  expect_error(birdnet_heatmap(data = valid_data, threshold = "high"), "numeric")
  expect_error(birdnet_heatmap(data = valid_data, threshold = 1.5), "1")
  expect_error(birdnet_heatmap(data = valid_data, threshold = c(0.1, 0.5)))
  expect_silent(birdnet_heatmap(data = valid_data, threshold = 0.7))

  df_threshold <- tibble(scientific_name = c("Turdus migratorius"), threshold = c(0.8))
  expect_silent(birdnet_heatmap(data = valid_data, threshold = df_threshold))

  df_bad_threshold <- tibble(name = "Turdus migratorius", value = 0.8)
  expect_error(birdnet_heatmap(data = valid_data, threshold = df_bad_threshold), "must.include")

  df_threshold_na <- tibble(scientific_name = c(NA), threshold = c(0.8))
  expect_error(birdnet_heatmap(data = valid_data, threshold = df_threshold_na), "missing")

  # 4. min_date and max_date: NULL or single character string
  expect_error(birdnet_heatmap(data = valid_data, min_date = 20230101))
  expect_error(birdnet_heatmap(data = valid_data, max_date = c("2023-01-01", "2023-01-02")))
  expect_silent(birdnet_heatmap(data = valid_data, min_date = "2023-01-01", max_date = "2023-12-31"))

  # 5. hour: NULL or numeric vector with values 0-23 no missing
  expect_error(birdnet_heatmap(data = valid_data, hour = -1))
  expect_error(birdnet_heatmap(data = valid_data, hour = 24))
  expect_error(birdnet_heatmap(data = valid_data, hour = c(5, NA)), "missing")
  expect_silent(birdnet_heatmap(data = valid_data, hour = c(0, 12, 23)))
})
