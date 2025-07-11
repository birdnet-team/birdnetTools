#' Calculate species-specific confidence thresholds for BirdNET detections
#'
#' Computes species-specific confidence thresholds for BirdNET detections using either a target
#' `precision` or a target predicted `probability` from a logistic regression model.
#' The function returns one threshold per species.
#' The `precision`-based approach follows [Tseng et al. (2025)](https://link.springer.com/article/10.1007/s10336-025-02260-w),
#' while the `probability`-based method is adapted from [Wood and Kahl (2024)](https://link.springer.com/article/10.1007/s10336-024-02144-5).
#'
#' @param validated_data A data frame of validated BirdNET detections with columns `common_name`,
#'   `confidence`, and `validation`. The `validation` column must contain 1 (true positives)
#'   and 0 (false positives).
#' @param full_data Optional. A data frame of all BirdNET detections with at least `common_name`
#'   and `confidence` columns. If `NULL`, `validated_data` is used instead.
#' @param probability Numeric. A target predicted probability (between 0 and 1) used to calculate thresholds
#'   from the logistic regression model.
#' @param precision Numeric. A target precision (between 0 and 1) used to select the lowest threshold
#'   that achieves the desired precision.
#'
#' @return A tibble with two columns:
#' \describe{
#'   \item{common_name}{Species common name.}
#'   \item{threshold}{The calculated confidence threshold for that species.}
#' }
#'
#' @details
#' You must supply exactly one of `precision` or `probability`. If both or neither are provided,
#' the function will throw an error.
#'
#' When using the precision method, the function predicts probabilities for each detection using a logistic
#' regression model fit to `validated_data`. It then identifies the lowest confidence threshold that meets
#' or exceeds the target precision.
#'
#' When using the probability method, the function calculates the confidence threshold corresponding to
#' the inverse logit of the target predicted probability from the regression model.
#'
#' All thresholds are clamped to fall between the minimum observed confidence in `validated_data`
#' and 1.
#'
#' @export
#' @references
#' Tseng, S., Hodder, D. P., & Otter, K. A. (2025). Setting BirdNET confidence thresholds: Species-specific vs. universal approaches. *Journal of Ornithology*. https://doi.org/10.1007/s10336-025-02260-w
#' Wood, C. M., & Kahl, S. (2024). Guidelines for appropriate use of BirdNET scores and other detector outputs. *Journal of Ornithology*. https://doi.org/10.1007/s10336-024-02144-5
birdnet_calc_threshold <- function(
    validated_data,
    full_data = NULL,
    probability = NULL,
    precision = NULL
) {

  # argument check ----------------------------------------------------------

  checkmate::assert_data_frame(validated_data, min.rows = 1)
  checkmate::assert_subset(c("common_name", "confidence", "validation"), colnames(validated_data),
                           .var.name = "validated_data")

  if (!all(unique(validated_data$validation) %in% c(0, 1))) {
    rlang::abort("The 'validation' column in `validated_data` must only contain 0 (false positives) and 1 (true positives).")
  }

  if (!is.null(full_data)) {
    checkmate::assert_data_frame(full_data, min.rows = 1)
    checkmate::assert_subset(c("common_name", "confidence"), colnames(full_data),
                             .var.name = "full_data")
  } else {
    full_data <- validated_data
  }

  # Make sure only one of probability or precision is specified
  if (!is.null(precision) && !is.null(probability)) {
    rlang::abort("You must specify only one of `precision` or `probability`, not both.")
  } else if (is.null(precision) && is.null(probability)) {
    rlang::abort("You must specify either `precision` or `probability`.")
  }

  # Check value bounds
  if (!is.null(probability)) {
    checkmate::assert_number(probability, lower = 0, upper = 1, na.ok = FALSE,
                             .var.name = "probability")
  }

  if (!is.null(precision)) {
    checkmate::assert_number(precision, lower = 0, upper = 1, na.ok = FALSE,
                             .var.name = "precision")
  }

  # helper functions --------------------------------------------------------

  threshold2precision <- function(probability_data, threshold_target) {
    probability_data |>
      dplyr::filter(confidence >= threshold_target) |>
      dplyr::pull(probability) |>
      mean()
  }

  precision2threshold <- function(threshold_table, precision_target) {
    filtered <- threshold_table |>
      dplyr::filter(precision >= precision_target)

    if (nrow(filtered) == 0) {
      return(NA_real_)
    }

    filtered |>
      dplyr::slice_min(threshold, with_ties = FALSE) |>
      dplyr::pull(threshold)
  }

  # species loop ------------------------------------------------------------

  species_list <- validated_data |>
    dplyr::distinct(common_name) |>
    dplyr::pull(common_name)

  pb <- cli::cli_progress_bar("Calculating thresholds",
                              total = length(species_list))

  results <- purrr::map_dfr(species_list, function(species_ind) {
    cli::cli_alert_info("Processing species: {.field {species_ind}}")

    species_data <- validated_data |>
      birdnet_filter_species(species = species_ind)

    model <- glm(validation ~ confidence,
                 family = binomial,
                 data = species_data)

    if (!is.null(precision)) {
      species_data_full <- full_data |>
        birdnet_filter_species(species = species_ind)

      species_data_full$probability <- predict(model,
                                               newdata = species_data_full,
                                               type = "response")


      threshold_table <- dplyr::tibble(threshold = seq(0, 1, 0.001)) |>
        dplyr::mutate(precision = purrr::map_dbl(threshold, ~ threshold2precision(species_data_full, .x)))

      t_target <- precision2threshold(threshold_table, precision)

    } else {
      t_target <- (log(probability / (1 - probability)) - coef(model)[1]) / coef(model)[2]
    }

    # Clamp to observed range
    min_conf <- min(validated_data$confidence, na.rm = TRUE)
    if (t_target < min_conf || t_target > 1) {
      t_target <- max(min_conf, min(t_target, 1))
      cli::cli_alert_warning(
        "Calculated threshold for {.val {species_ind}} is outside [0, 1] range and has been clamped."
      )
    }

    dplyr::tibble(
      common_name = species_ind,
      threshold = t_target
    )
  })

  if (!is.null(precision)) {
    cli::cli_alert_success("Thresholds calculated to achieve {.val {precision}} precision.")
  } else {
    cli::cli_alert_success("Thresholds calculated to correspond to {.val {probability}} predicted probability.")
  }

  return(results)
}

