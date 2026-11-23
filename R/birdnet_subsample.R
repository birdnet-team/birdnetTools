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
#' @param quiet Logical. If `TRUE`, suppresses the status messages about how
#'   many detections were returned and whether all of them were kept because
#'   `data` had no more than `n` rows. Defaults to `FALSE`.
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
                              file = NULL,
                              quiet = FALSE) {


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
  conf <- cols$confidence
  n_take <- min(n, nrow(data))

  run <- function() {

    if (method == "random") {
      return(dplyr::slice_sample(data, n = n_take))
    }

    if (method == "top") {
      return(dplyr::slice_max(data, order_by = .data[[conf]],
                              n = n_take, with_ties = FALSE))
    }

    # stratified methods: bins cover 0-1 so no confidence value becomes NA
    breaks <- unique(c(seq(0, 1, by = bin_width), 1))
    bins <- split(
      seq_len(nrow(data)),
      cut(data[[conf]], breaks = breaks, include.lowest = TRUE),
      drop = TRUE
    )
    counts <- lengths(bins)

    alloc <- if (method == "even_stratified") {
      allocate_even(counts, n)
    } else {
      allocate_proportional(counts, n)
    }

    rows <- unlist(
      Map(function(idx, k) idx[sample.int(length(idx), k)], bins, alloc),
      use.names = FALSE
    )
    data[rows, , drop = FALSE]
  }

  # a seed makes the sample reproducible without touching the user's RNG state
  sampled_data <- if (is.null(seed)) run() else withr::with_seed(seed, run())

  # tell the user what they got ----------------------------------------------

  if (!quiet) {

    cli::cli_alert_success("Returned {nrow(sampled_data)} detection{?s}.")

    if (nrow(data) <= n) {
      cli::cli_alert_info(
        "`data` has {nrow(data)} detection{?s}, which is not more than {.arg n}, so all were kept."
      )
    }
  }

  # optional write to CSV (as before) ----------------------------------------
  if (save_to_file || !is.null(file)) {
    file_to_write <- if (is.null(file)) "subsampled_data.csv" else file
    readr::write_csv(sampled_data, file_to_write, progress = FALSE)
  }

  return(sampled_data)
}
