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
#' birdnet_subsample(data = my_data, n = 100, method = "top", save_to_file = TRUE, file = "top_samples.csv")
#' }
#'
#' @export
birdnet_subsample <- function(
    data,
    n,
    method = c("stratified", "random", "top"),
    save_to_file = FALSE,
    file = NULL) {
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
  }

  # main function -----------------------------------------------------------

  # check if the required columns are present
  cols <- birdnet_detect_columns(data)

  # balancing samples with different confidence scores
  if (method == "stratified") {
    data <- data |>
      dplyr::mutate(category = cut(!!dplyr::sym(cols$confidence),
        breaks = seq(0.1, 1, by = 0.05),
        right = FALSE
      )) |>
      dplyr::slice_sample(
        n = round(n / 18),
        by = c(category, !!dplyr::sym(cols$confidence))
      ) |>
      dplyr::select(-category)

    # random sampling the data
  } else if (method == "random") {
    data <- data |>
      dplyr::slice_sample(
        n = n,
        replace = FALSE,
        by = !!dplyr::sym(cols$common_name)
      )

    # useful when checking the richness
  } else if (method == "top") {
    data <- data |>
      dplyr::slice_max(!!dplyr::sym(cols$confidence),
        n = n,
        by = !!dplyr::sym(cols$common_name)
      )
  }

  if (!is.null(file) || save_to_file) {
    file_to_write <- if (is.null(file)) "subsampled_data.csv" else file
    readr::write_csv(data, file = file_to_write)
  }

  return(data)
}
