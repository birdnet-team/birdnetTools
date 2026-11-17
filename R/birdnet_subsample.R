#' Subsample BirdNET detections by species
#'
#' Draws a subsample of `n` detections from a BirdNET output data frame, for
#' example for manual validation. Sampling is done across all rows of `data`.
#' To subsample one species, filter to that species first (see Examples).
#' Four methods are available: even or proportional stratified sampling across
#' confidence score bins, simple random sampling, or the highest-confidence
#' detections. Optionally saves the subsample to a CSV file.
#'
#' @param data A data frame containing BirdNET output.
#' @param n Integer. Total number of detections to subsample. If `data` has
#'   `n` or fewer rows, all rows are returned.
#' @param method Character. Subsampling method to use. One of:
#'   \describe{
#'     \item{"even_stratified"}{Samples evenly across confidence score bins.
#'     Bins with too few detections contribute all they have, and the remainder
#'     is shared among the other bins.}
#'     \item{"proportional_stratified"}{Samples across confidence score bins in
#'     proportion to the number of detections in each bin.}
#'     \item{"random"}{Simple random sample of `n` detections.}
#'     \item{"top"}{The `n` detections with the highest confidence.}
#'   }
#'   Defaults to `"even_stratified"`.
#' @param bin_width Numeric. Width of the confidence score bins used by the
#'   stratified methods. Defaults to `0.05`.
#' @param seed Integer or `NULL`. If given, the subsample is reproducible. The
#'   random number generator state of your session is left unchanged.
#' @param save_to_file Logical. If `TRUE`, saves the output to a CSV file.
#'   Defaults to `FALSE`. Automatically set to `TRUE` if `file` is provided.
#' @param file Character or `NULL`. File path to save the output CSV.
#'   If `NULL` and `save_to_file = TRUE`, saves as `"subsampled_data.csv"` in the working directory.
#'
#'#' @details
#' **Choosing a method.**
#' * `"random"` gives an unbiased sample, suited to estimating overall precision.
#' * `"even_stratified"` gives balanced coverage across confidence scores, suited
#'   to relating precision to score or choosing a threshold.
#' * `"proportional_stratified"` mirrors the confidence distribution of the
#'   detections, so it is dominated by low-confidence detections.
#' * `"top"` is useful for confirming that a species is present, but it is
#'   biased for estimating precision.
#'
#' **Several species.** Sampling is not done per species. To subsample each
#' species separately, apply the function to each species in turn (see
#' Examples).
#'
#' @return A data frame containing the subsampled observations.
#'
#' @examples
#' \dontrun{
#' # one species: filter first, then subsample
#' my_data |>
#'   dplyr::filter(common_name == "Barred Owl") |>
#'   birdnet_subsample(n = 100, method = "even_stratified", seed = 1)
#'
#' # several species: apply the function to each one
#' my_data |>
#'   split(my_data$common_name) |>
#'   purrr::map(birdnet_subsample, n = 100) |>
#'   dplyr::bind_rows()
#' }
#'
#' @export


birdnet_subsample <- function(data,
                              n,
                              method = c("even_stratified", "proportional_stratified", "random", "top"),
                              bin_width = 0.05,
                              seed = NULL,
                              save_to_file = FALSE,
                              file = NULL) {


  # argument check ----------------------------------------------------------

  checkmate::assert_data_frame(data)
  checkmate::assert_count(n, positive = TRUE)
  method <- match.arg(method)
  checkmate::assert_number(bin_width, lower = 0.001, upper = 0.5)
  checkmate::assert_int(seed, null.ok = TRUE)
  checkmate::assert_flag(save_to_file)

  checkmate::assert_choice(method, choices = c("even_stratified", "proportional_stratified", "random", "top"))

  # save_to_file is a logical value
  checkmate::assert_flag(save_to_file)

  # If file is not NULL, check that it’s a character string ending in ".csv"
  if (!is.null(file)) {
    checkmate::assert_character(file, len = 1, any.missing = FALSE)
    if (!grepl("\\.csv$", file, ignore.case = TRUE)) {
      cli::cli_abort("{.arg file} must end with {.val .csv}.")
    }
    save_to_file <- TRUE
  }


  # main function -----------------------------------------------------------

  # check if the required columns are present
  cols <- birdnet_detect_columns(data)
  total_rows <- nrow(data)

  if (total_rows <= n) {
    sampled_data <- data

  } else if (method == "random") {
    sampled_data <- dplyr::slice_sample(data, n = n,
                                        replace = FALSE)

  } else if (method == "top") {
    sampled_data <- dplyr::slice_max(data, order_by = !!dplyr::sym(cols$confidence),
                                     n = n, with_ties = FALSE)

  } else if (method == "even_stratified") {
    # assign bins
    data <- data |> dplyr::mutate(category = cut(!!dplyr::sym(cols$confidence),
                                                 breaks = seq(0.1, 1, by = 0.05),
                                                 right = FALSE))
    n_bins <- length(unique(data$category))
    n_per_bin <- floor(n / n_bins)

    # sample from each bin evenly
    sampled_data <- dplyr::bind_rows(lapply(unique(data$category), function(bin) {
      bin_data <- dplyr::filter(data, category == bin)
      if (nrow(bin_data) <= n_per_bin) {
        # take all if not enough rows
        bin_data
      } else {
        dplyr::slice_sample(bin_data, n = n_per_bin)
      }
    })) |>
      dplyr::select(-category)


  } else if (method == "proportional_stratified") {
    # assign bins
    data <- data |> dplyr::mutate(category = cut(!!dplyr::sym(cols$confidence),
                                                 breaks = seq(0.1, 1, by = 0.05),
                                                 right = FALSE))

    # count per bin
    bin_counts <- data |>
      dplyr::group_by(category) |>
      dplyr::summarise(count = dplyr::n(), .groups = "drop")

    # proportional allocation
    bin_counts <- bin_counts |>
      dplyr::mutate(prop_sample = n * count / sum(count),
                    sample_n = floor(prop_sample),
                    frac = prop_sample - sample_n)

    # leftover allocation by largest fractional part
    leftover <- n - sum(bin_counts$sample_n)
    if (leftover > 0) {
      bin_counts <- bin_counts |>
        dplyr::arrange(dplyr::desc(frac))
      for (i in seq_len(leftover)) {
        bin_counts$sample_n[i] <- bin_counts$sample_n[i] + 1
      }
    }

    # don't oversample bins
    bin_counts <- bin_counts |>
      dplyr::mutate(sample_n = pmin(sample_n, count))

    # sample from bins
    sampled_data <- dplyr::bind_rows(lapply(seq_len(nrow(bin_counts)), function(i) {
      bin <- bin_counts$category[i]
      n_bin <- bin_counts$sample_n[i]
      if (n_bin <= 0) return(NULL)
      bin_data <- dplyr::filter(data, category == bin)
      if (nrow(bin_data) <= n_bin) bin_data else dplyr::slice_sample(bin_data, n = n_bin)
    })) |> dplyr::select(-category)
  }


  # optional write to CSV
  if (save_to_file) {
    file_to_write <- if (is.null(file)) "subsampled_data.csv" else file
    readr::write_csv(sampled_data, file_to_write)
  }

  return(sampled_data)
}
