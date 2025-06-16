#' Calculate species-specific confidence thresholds for BirdNET detections
#'
#' Calculates species-specific confidence thresholds for BirdNET detections based on either a target
#' \code{precision} or a predicted \code{probability} from a logistic regression model. Returns a tibble
#' with one threshold per species. The \code{precision} approach is based on [Tseng et al., 2025](https://link.springer.com/article/10.1007/s10336-025-02260-w),
#' while the \code{probability} approach is based on [Wood and Kahl, 2024](https://link.springer.com/article/10.1007/s10336-024-02144-5).
#'
#' @param validated_data A data frame of validated BirdNET detections with columns \code{common_name},
#' \code{confidence}, and \code{validation}. The \code{validation} column must contain 1 (true positives)
#' and 0 (false positives).
#' @param full_data Optional. A data frame of all BirdNET detections. Must contain at least \code{common_name}
#' and \code{confidence} columns. If \code{NULL}, \code{validated_data} will be used.
#' @param probability Target predicted probability (between 0 and 1) used to calculate the threshold from the logistic regression.
#' @param precision Target precision (between 0 and 1) used to select the threshold achieving that precision.
#'
#' @return A tibble with two columns:
#' \describe{
#'   \item{common_name}{Species common name}
#'   \item{threshold}{Calculated confidence threshold}
#' }
#'
#' @details
#' If \code{precision} is provided, thresholds are selected based on the lowest confidence score that achieves or exceeds
#' the desired precision when applied to predicted probabilities. If \code{probability} is provided instead, thresholds are calculated
#' directly from the inverse logit of the logistic regression model. You must provide exactly one of \code{precision} or \code{probability}.
#'
#' All calculated thresholds are clamped to the range between the minimum observed confidence
#' in \code{validated_data} and 1.
#'
#' @examples
#' \dontrun{
#' birdnet_calc_threshold(
#'   validated_data = validated_df,
#'   full_data = full_df,
#'   precision = 0.9
#' )
#' }
#'
#' @export
birdnet_calc_threshold <- function(validated_data,
                                   full_data = NULL,
                                   probability = NULL,
                                   precision = NULL) {

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

      predicted <- species_data_full |>
        dplyr::mutate(probability = predict(model,
                                            newdata = species_data_full,
                                            type = "response"))

      threshold_table <- dplyr::tibble(threshold = seq(0, 1, 0.001)) |>
        dplyr::mutate(precision = purrr::map_dbl(threshold, ~ threshold2precision(predicted, .x)))

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





#' #' Calculate species-specific confidence thresholds for BirdNET detections
#' #'
#' #' Calculates species-specific confidence thresholds for BirdNET detections based on either a target
#' #' \code{precision} or a predicted \code{probability} from a logistic regression model. Returns a tibble
#' #' with one threshold per species. The method for the \code{"precision"} approach is based on [Tseng et al., 2025](https://link.springer.com/article/10.1007/s10336-025-02260-w),
#' #' while the \code{"probability"} approach is based on [Wood and Kahl, 2024](https://link.springer.com/article/10.1007/s10336-024-02144-5).
#' #'
#' #' @param validated_data A data frame of validated BirdNET detections with columns \code{common_name},
#' #' \code{confidence}, and \code{validation}. The \code{validation} column must contain 1 (true positives)
#' #' and 0 (false positives).
#' #' @param full_data Optional. A data frame of all BirdNET detections. Must contain at least \code{common_name}
#' #' and \code{confidence} columns. If \code{NULL}, \code{validated_data} will be used.
#' #' @param method Method for threshold calculation. Either \code{"precision"} (default), which finds
#' #' the minimum threshold achieving the target precision, or \code{"probability"}, which finds the threshold
#' #' associated with the predicted probability from the logistic regression.
#' #' @param probability Target predicted probability (between 0 and 1) used when \code{method = "probability"}.
#' #' @param precision Target precision (between 0 and 1) used when \code{method = "precision"}.
#' #'
#' #' @return A tibble with two columns:
#' #' \describe{
#' #'   \item{common_name}{Species common name}
#' #'   \item{threshold}{Calculated confidence threshold}
#' #' }
#' #'
#' #' @details
#' #' When using \code{method = "precision"}, a logistic regression model is fitted for each species
#' #' using \code{validated_data}. Thresholds are evaluated across a range of values, and the lowest
#' #' threshold that meets or exceeds the target precision is selected. If no such threshold exists,
#' #' \code{NA} is returned for that species.
#' #'
#' #' When using \code{method = "probability"}, the inverse logit function is used to solve for the
#' #' confidence score that gives the desired probability of detection.
#' #'
#' #' All calculated thresholds are clamped to the range between the minimum observed confidence
#' #' in \code{validated_data} and 1.
#' #'
#' #' @examples
#' #' \dontrun{
#' #' birdnet_calc_threshold(
#' #'   validated_data = validated_df,
#' #'   full_data = full_df,
#' #'   method = "precision",
#' #'   precision = 0.9
#' #' )
#' #' }
#' #'
#' #' @export
#' birdnet_calc_threshold <- function(validated_data,
#'                                    full_data = NULL,
#'                                    method = c("precision", "probability"),
#'                                    probability = NULL,
#'                                    precision = NULL) {
#'
#'   # argument check ----------------------------------------------------------
#'
#'   # Check validated_data columns
#'   checkmate::assert_data_frame(validated_data, min.rows = 1)
#'   checkmate::assert_subset(c("common_name", "confidence", "validation"), colnames(validated_data),
#'                 .var.name = "validated_data")
#'
#'   # Check that 'validation' contains only 0 and 1
#'   unique_vals <- unique(validated_data$validation)
#'   if (!all(unique_vals %in% c(0, 1))) {
#'     rlang::abort("The 'validation' column in `validated_data` must only contain 0 (false positives) and 1 (true positives).")
#'   }
#'
#'   # Check full_data if provided
#'   if (!is.null(full_data)) {
#'     checkmate::assert_data_frame(full_data, min.rows = 1)
#'     checkmate::assert_subset(c("common_name", "confidence"), colnames(full_data),
#'                   .var.name = "full_data")
#'   } else {
#'     full_data <- validated_data  # fallback
#'   }
#'
#'   # Validate numeric bounds using checkmate
#'   if (!is.null(probability)) {
#'     checkmate::assert_number(probability, lower = 0, upper = 1, na.ok = FALSE,
#'                   .var.name = "probability")
#'   }
#'
#'   if (!is.null(precision)) {
#'     checkmate::assert_number(precision, lower = 0, upper = 1, na.ok = FALSE,
#'                   .var.name = "precision")
#'   }
#'
#'
#'   # Handle method override logic
#'   method_input <- match.arg(method)
#'
#'   if (!is.null(precision) && !is.null(probability)) {
#'     rlang::abort("You must specify only one of `precision` or `probability`, not both.")
#'   }
#'
#'   # Override method based on non-NULL values
#'   if (!is.null(precision)) {
#'     method <- "precision"
#'   } else if (!is.null(probability)) {
#'     method <- "probability"
#'   } else {
#'     # fallback if none are specified
#'     rlang::abort("You must specify either `precision` or `probability`.")
#'   }
#'
#'
#'   # add progress bar
#'
#'
#'   # helper function ---------------------------------------------------------
#'
#'   # find precision given a threshold
#'   threshold2precision <- function(probability_data,
#'                                   threshold_target) {
#'     probability_data |>
#'       dplyr::filter(confidence >= threshold_target) |>
#'       dplyr::pull(probability) |>
#'       mean()
#'   }
#'
#'   # function to determine the threshold given specified precision level
#'   precision2threshold <- function(threshold_table,
#'                                   precision_target) {
#'     filtered <- threshold_table |>
#'       dplyr::filter(precision >= precision_target)
#'
#'     if (nrow(filtered) == 0) {
#'       return(NA_real_)
#'     }
#'
#'     filtered |>
#'       dplyr::slice_min(threshold, with_ties = FALSE) |>
#'       dplyr::pull(threshold)
#'   }
#'
#'
#'
#'   # main function -----------------------------------------------------------
#'
#'   # filter out data to keep only interested species and find the species names
#'   species_list <- validated_data |>
#'     dplyr::distinct(common_name) |>
#'     dplyr::pull(common_name)
#'
#'   # create a progress bar
#'   pb <- cli::cli_progress_bar("Calculating thresholds",
#'                               total = length(species_list))
#'
#'   # start looping through the species using purrr::map_dfr
#'   results <- purrr::map_dfr(species_list, function(species_ind) {
#'
#'     cli::cli_alert_info("Processing species: {.field {species_ind}}")
#'
#'     # individual species data
#'     species_data <- validated_data |>
#'       birdnet_filter_species(species = species_ind)
#'
#'     # individual species model
#'     species_model <- glm(
#'       formula = validation ~ confidence,
#'       family = binomial,
#'       data = species_data
#'     )
#'
#'     # # indicate if the model is with a negative slope
#'     # if (is.na(coef(model)[2]) && coef(model)[2] <= 0) {
#'     #   cli::cli_alert_warning(
#'     #     "Species {.val {species_name}} shows a weak or negative relationship between confidence and validation. Thresholds may not be reliable."
#'     #   )
#'     # }
#'
#'
#'     # find threshold based on the precision
#'     if (method == "precision") {
#'       species_data_full <- full_data |>
#'         birdnet_filter_species(species = species_ind)
#'
#'       species_probability <- species_data_full |>
#'         dplyr::mutate(probability = predict(species_model,
#'                                             newdata = species_data_full,
#'                                             type = "response"))
#'
#'       threshold_table <- dplyr::tibble(threshold = seq(0, 1, 0.001)) |>
#'         dplyr::mutate(precision = purrr::map_dbl(threshold, ~ threshold2precision(species_probability, .x)))
#'
#'       t_target <- precision2threshold(threshold_table = threshold_table,
#'                                       precision_target = precision)
#'
#'     # find threshold based on probability
#'     } else if (method == "probability") {
#'
#'       t_target <- (log(probability / (1 - probability)) - coef(species_model)[1]) / coef(species_model)[2]
#'
#'     }
#'
#'     # clamp the threshold to [0, 1] range
#'     if (t_target < min(validated_data$confidence, na.rm = TRUE) || t_target > 1) {
#'
#'       t_target <- max(min(validated_data$confidence, na.rm = TRUE),
#'                       min(t_target, 1)
#'       )
#'       cli::cli_alert_warning(
#'         "Calculated threshold for {.val {species_ind}} is outside [0, 1] range and has been clamped."
#'       )
#'     }
#'
#'     # combine the results into one single dataframe
#'     dplyr::tibble(
#'       common_name = species_ind,
#'       threshold = t_target
#'     )
#'   })
#'
#'
#'   # final output message to remind the set value
#'   if (method == "precision") {
#'     cli::cli_alert_success("Thresholds calculated to achieve {.val {precision}} precision.")
#'   } else {
#'     cli::cli_alert_success("Thresholds calculated to correspond to {.val {probability}} predicted probability.")
#'   }
#'
#'   return(results)
#' }
