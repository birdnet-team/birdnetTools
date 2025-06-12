#' Calculate species-specific confidence thresholds for BirdNET detections
#'
#' Calculates species-specific confidence thresholds for BirdNET detections based on either a target
#' \code{precision} or a predicted \code{probability} from a logistic regression model. Returns a tibble
#' with one threshold per species. The method for the \code{"precision"} approach is based on [Tseng et al., 2025](https://link.springer.com/article/10.1007/s10336-025-02260-w),
#' while the \code{"probability"} approach is based on [Wood and Kahl, 2024](https://link.springer.com/article/10.1007/s10336-024-02144-5).
#'
#' @param validated_data A data frame of validated BirdNET detections with columns \code{common_name},
#' \code{confidence}, and \code{validation}. The \code{validation} column must contain 1 (true positives)
#' and 0 (false positives).
#' @param full_data Optional. A data frame of all BirdNET detections. Must contain at least \code{common_name}
#' and \code{confidence} columns. If \code{NULL}, \code{validated_data} will be used.
#' @param method Method for threshold calculation. Either \code{"precision"} (default), which finds
#' the minimum threshold achieving the target precision, or \code{"probability"}, which finds the threshold
#' associated with the predicted probability from the logistic regression.
#' @param probability Target predicted probability (between 0 and 1) used when \code{method = "probability"}.
#' Default is 0.9.
#' @param precision Target precision (between 0 and 1) used when \code{method = "precision"}. Default is 0.9.
#'
#' @return A tibble with two columns:
#' \describe{
#'   \item{common_name}{Species common name}
#'   \item{threshold}{Calculated confidence threshold}
#' }
#'
#' @details
#' When using \code{method = "precision"}, a logistic regression model is fitted for each species
#' using \code{validated_data}. Thresholds are evaluated across a range of values, and the lowest
#' threshold that meets or exceeds the target precision is selected. If no such threshold exists,
#' \code{NA} is returned for that species.
#'
#' When using \code{method = "probability"}, the inverse logit function is used to solve for the
#' confidence score that gives the desired probability of detection.
#'
#' All calculated thresholds are clamped to the range between the minimum observed confidence
#' in \code{validated_data} and 1.
#'
#' @examples
#' \dontrun{
#' birdnet_calc_threshold(
#'   validated_data = validated_df,
#'   full_data = full_df,
#'   method = "precision",
#'   precision = 0.9
#' )
#' }
#'
#' @export
birdnet_calc_threshold <- function(validated_data,
                                   full_data = NULL,
                                   method = c("precision", "probability"),
                                   probability = 0.9,
                                   precision = 0.9) {
  # argument check ----------------------------------------------------------

  method <- match.arg(method)

  # validated data has to have at least a column called "validation",
  # and "1" and "0" as values to indicate true positive or false positive of the detection

  # if selecting method = precision, then precision has should be supplied
  # if selecting method = probability, then probability should be supplied

  # method has to be either one of the two options


  if (is.null(full_data)) {
    full_data <- validated_data
  }


  # helper function ---------------------------------------------------------

  # find precision given a threshold
  threshold2precision <- function(probability_data,
                                  threshold_target) {
    probability_data |>
      dplyr::filter(confidence >= threshold_target) |>
      dplyr::pull(probability) |>
      mean()
  }

  # function to determine the threshold given specified precision level
  precision2threshold <- function(threshold_table,
                                  precision_target) {
    filtered <- threshold_table |>
      dplyr::filter(precision >= precision_target)

    if (nrow(filtered) == 0) {
      return(NA_real_)
    }

    filtered %>%
      dplyr::slice_min(threshold, with_ties = FALSE) %>%
      dplyr::pull(threshold)
  }



  # main function -----------------------------------------------------------

  # filter out data to keep only interested species and find the species names
  species_list <- validated_data |>
    dplyr::distinct(common_name) |>
    dplyr::pull(common_name)


  pb <- cli::cli_progress_bar("Calculating thresholds", total = length(species_list))

  results <- purrr::map_dfr(species_list, function(species_ind) {
    cli::cli_alert_info("Processing species: {.field {species_ind}}")

    species_data <- validated_data |>
      filter(common_name == species_ind)

    species_model <- glm(
      formula = validation ~ confidence,
      family = binomial,
      data = species_data
    )

    if (method == "precision") {
      species_data_full <- full_data |>
        filter(common_name == species_ind)

      species_probability <- species_data_full |>
        mutate(probability = predict(species_model, newdata = species_data_full, type = "response"))

      threshold_table <- tibble(threshold = seq(0, 1, 0.001)) |>
        mutate(precision = map_dbl(threshold, ~ threshold2precision(species_probability, .x)))

      t_target <- precision2threshold(threshold_table, precision_target = precision)

    } else if (method == "probability") {
      t_target <- (log(probability / (1 - probability)) - coef(species_model)[1]) / coef(species_model)[2]
    }

    if (t_target < min(validated_data$confidence, na.rm = TRUE) || t_target > 1) {
      t_target <- max(
        min(validated_data$confidence, na.rm = TRUE),
        min(t_target, 1)
      )
      cli::cli_alert_warning(
        "Calculated threshold for {.val {species_ind}} is outside [0, 1] range and has been clamped."
      )
    }

    tibble(
      common_name = species_ind,
      threshold = t_target
    )
  })

  if (method == "precision") {
    cli::cli_alert_success("Thresholds calculated to achieve {.val {precision}} precision.")
  } else {
    cli::cli_alert_success("Thresholds calculated to correspond to {.val {probability}} predicted probability.")
  }

  return(results)
}

  # # start the for loop across the species
  # thresholds <- numeric(length(species_list))
  # names(thresholds) <- species_list
  #
  # pb <- cli::cli_progress_bar("Calculating thresholds", total = length(species_list))
  #
  # for (species_ind in species_list) {
  #   cli::cli_alert_info("Processing species: {.field {species_ind}}")
  #   cli::cli_progress_update()
  #
  #   # get the species model ready
  #   species_data <- validated_data |>
  #     filter(common_name == species_ind)
  #
  #   species_model <- species_data |>
  #     glm(
  #       formula = validation ~ confidence,
  #       family = binomial
  #     )
  #
  #
  #
  #   if (method == "precision") {
  #     # use the built model to covert confidence to probability
  #     species_data_full <- full_data |>
  #       filter(common_name == species_ind)
  #
  #     species_probability <- species_data_full |>
  #       mutate(probability = predict(species_model,
  #         newdata = species_data_full,
  #         type = "response"
  #       ))
  #
  #     # create a table of threshlds with precision
  #     threshold_table <- tibble(threshold = seq(0, 1, 0.001)) |>
  #       mutate(precision = map_dbl(
  #         .x = threshold,
  #         .f = ~ threshold2precision(species_probability, .x)
  #       ))
  #
  #     # find the threshold that gives the target precision
  #     t_target <- precision2threshold(
  #       threshold_table = threshold_table,
  #       precision_target = precision
  #     )
  #   } else if (method == "probability") {
  #     # use the built model to convert confidence to probability (using validation dataset)
  #     t_target <- (log(probability / (1 - probability)) - species_model$coefficients[1]) / species_model$coefficients[2]
  #   }
  #
  #
  #   if (t_target < min(validated_data$confidence, na.rm = TRUE) || t_target > 1) {
  #     # clamp the threshold to [0, 1] range
  #     t_target <- max(
  #       min(validated_data$confidence, na.rm = TRUE),
  #       min(t_target, 1)
  #     )
  #
  #     cli::cli_alert_warning(
  #       "Calculated threshold for {.val {species_ind}} is outside [0, 1] range and has been clamped."
  #     )
  #   }
  #
  #   # assign the threshold to the species
  #   thresholds[species_ind] <- t_target
  # }
  #
  # cli::cli_progress_done()
  #
  # if (method == "precision") {
  #   cli::cli_alert_success("Thresholds calculated to achieve at least {.val {precision}} precision.")
  # } else if (method == "probability") {
  #   cli::cli_alert_success("Thresholds calculated to correspond to {.val {probability}} predicted probability.")
  # }
  #
  # return(tibble(
  #   common_name = names(thresholds),
  #   threshold = thresholds
  # ))
