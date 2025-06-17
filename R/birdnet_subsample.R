#' Subsample BirdNET detections by species
#'
#' Subsamples a specified number of observations for a given species from a BirdNET output dataset
#' using one of three methods: stratified, random, or top confidence. Optionally saves the result to a CSV file.
#'
#' @param data A data frame containing BirdNET output. It must include at least the columns `common_name` and `confidence`.
#' @param n Integer. Total number of observations to subsample for each species in `data`.
#' @param method Character string. Subsampling method to use. One of `"stratified"`, `"random"`, or `"top"`:
#' \describe{
#'   \item{`"stratified"`}{Samples across confidence score strata (0.1 to 1 by 0.05 bins) evenly.}
#'   \item{`"random"`}{Randomly samples `n` observations.}
#'   \item{`"top"`}{Selects the top `n` observations with the highest confidence.}
#' }
#' @param save_to_file Logical. If `TRUE`, saves the output data frame to a file named `"subsampled_data.csv"` in the working directory. Default is `FALSE`. Automatically set to `TRUE` if `path` is defined.
#' @param file Character string or `NULL`. File path to save the output.
#' If `NULL` and `save_to_file = TRUE`, the file is saved as "subsampled_data.csv"
#' in the working directory.
#'
#' @return A data frame containing the subsampled observations.
#'
#' @examples
#' \dontrun{
#' birdnet_subsample(data = my_data, species = "American Robin", n = 300, method = "stratified")
#' }
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

  data <- data |>
    birdnet_clean_names()

  # balancing samples with different confidence scores
  if (method == "stratified") {
    data <- data |>
      dplyr::mutate(category = cut(confidence,
        breaks = seq(0.1, 1, by = 0.05),
        right = FALSE
      )) |>
      dplyr::slice_sample(
        n = round(n / 18),
        by = c(category, common_name)
      ) |>
      dplyr::select(-category)

    # random sampling the data
  } else if (method == "random") {
    data <- data |>
      dplyr::slice_sample(
        n = n,
        replace = FALSE,
        by = common_name
      )

    # useful when checking the richness
  } else if (method == "top") {
    data <- data |>
      dplyr::slice_max(confidence,
        n = n,
        by = common_name
      )
  }

  if (!is.null(file) || save_to_file) {
    file_to_write <- if (is.null(file)) "subsampled_data.csv" else file
    readr::write_csv(data, file = file_to_write)
  }

  return(data)
}
