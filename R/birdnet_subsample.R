#' Subsample BirdNET detections by species
#'
#' Subsamples a specified number of observations per species from a BirdNET output dataset.
#' Supports three subsampling methods: stratified by confidence score bins, random, or top confidence scores.
#' Optionally saves the subsampled data to a CSV file.
#'
#' @param data A data frame containing BirdNET output. Relevant columns
#'   (e.g., common name, confidence, datetime) are automatically detected
#'   by [birdnet_detect_columns].
#' @param n Integer. Number of observations to subsample **per species**.
#' @param method Character. Subsampling method to use. One of:
#'   \describe{
#'     \item{"stratified"}{Samples evenly across confidence score strata (0.1 to 1 by 0.05 bins).}
#'     \item{"random"}{Randomly samples `n` observations per species.}
#'     \item{"top"}{Selects the top `n` observations with the highest confidence per species.}
#'   }
#'   Defaults to `"stratified"`.
#' @param save_to_file Logical. If `TRUE`, saves the output to a CSV file.
#'   Defaults to `FALSE`. Automatically set to `TRUE` if `file` is provided.
#' @param file Character or `NULL`. File path to save the output CSV.
#'   If `NULL` and `save_to_file = TRUE`, saves as `"subsampled_data.csv"` in the working directory.
#'
#' @return A data frame containing the subsampled observations.
#'
#' @examples
#' \dontrun{
#' birdnet_subsample(data = my_data, n = 300, method = "stratified")
#' birdnet_subsample(data = my_data, n = 100, method = "top", save_to_file = TRUE,
#' file = "top_samples.csv")
#' }
#'
#' @export


birdnet_subsample <- function(
    data,
    n,
    method = c("stratified", "random", "top"),
    save_to_file = FALSE,
    file = NULL
) {


  # argument check ----------------------------------------------------------

  # data is a dataframe
  checkmate::assert_data_frame(data)

  # n is a single positive integer
  checkmate::assert_count(n, positive = TRUE)

  # method is one of the accepted values
  method <- match.arg(method)
  checkmate::assert_choice(method, choices = c("stratified", "random", "top"))

  # save_to_file is a logical value
  checkmate::assert_flag(save_to_file)

  # If file is not NULL, check that it’s a character string ending in ".csv"
  if (!is.null(file)) {
    checkmate::assert_character(file, len = 1, any.missing = FALSE)
    if (!grepl("\\.csv$", file, ignore.case = TRUE)) {
      rlang::abort("`file` must end with '.csv'.")
    }
    save_to_file <- TRUE
  }

  # main function -----------------------------------------------------------

  # check if the required columns are present
  cols <- birdnet_detect_columns(data)
  conf_col <- cols$confidence

  total_rows <- nrow(data)
  if (total_rows <= n) {
    sampled_data <- data
  } else if (method == "random") {
    sampled_data <- dplyr::slice_sample(data, n = n, replace = FALSE)
  } else if (method == "top") {
    sampled_data <- dplyr::slice_max(data, order_by = !!rlang::sym(conf_col),
                                     n = n, with_ties = FALSE)
  } else if (method == "stratified") {
    # assign bins
    data <- dplyr::mutate(
      data,
      category = cut(
        !!rlang::sym(conf_col),
        breaks = seq(0.1, 1, by = 0.05),
        right = FALSE
      )
    )

    # count per bin
    bin_counts <- data %>%
      dplyr::group_by(category) %>%
      dplyr::summarise(count = dplyr::n(), .groups = "drop")

    # proportional allocation
    bin_counts <- bin_counts %>%
      dplyr::mutate(prop_sample = n * count / sum(count),
                    sample_n = floor(prop_sample),
                    frac = prop_sample - sample_n)

    # leftover allocation by largest fractional part
    leftover <- n - sum(bin_counts$sample_n)
    if (leftover > 0) {
      bin_counts <- bin_counts %>%
        dplyr::arrange(dplyr::desc(frac))
      for (i in seq_len(leftover)) {
        bin_counts$sample_n[i] <- bin_counts$sample_n[i] + 1
      }
    }

    # don't oversample bins
    bin_counts <- bin_counts %>%
      dplyr::mutate(sample_n = pmin(sample_n, count))

    # sample from bins
    sampled_data <- dplyr::bind_rows(lapply(seq_len(nrow(bin_counts)), function(i) {
      bin <- bin_counts$category[i]
      n_bin <- bin_counts$sample_n[i]
      if (n_bin <= 0) return(NULL)
      bin_data <- dplyr::filter(data, category == bin)
      if (nrow(bin_data) <= n_bin) bin_data else dplyr::slice_sample(bin_data, n = n_bin)
    })) %>% dplyr::select(-category)
  }

  # optional write to CSV
  if (save_to_file) {
    file_to_write <- if (is.null(file)) "subsampled_data.csv" else file
    readr::write_csv(sampled_data, file_to_write)
  }

  return(sampled_data)
}
