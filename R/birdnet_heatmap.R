#' Create a heatmap of BirdNET detections by hour and date
#'
#' This function generates a heatmap showing the daily activity pattern of a specified species
#' detected in BirdNET output data. The function optionally filters detections by a confidence
#' threshold and displays activity levels (number of detections) across time.
#'
#' @param data A data frame containing BirdNET output. Must include columns with time and species
#'   information. Column names will be standardized internally using [birdnet_clean_names()].
#' @param species Character. The common name of the species to visualize.
#' @param threshold Numeric or `NULL`. Optional confidence threshold (0–1) used to filter detections.
#'   If `NULL`, no threshold filtering is applied.
#' @param hour_range A numeric vector of length 2 indicating the y-axis limits for hour-of-day
#'   display. Default is `c(0, 23)`.
#'
#' @return A `ggplot` heatmap showing the number of detections by hour and date.
#' @examples
#' \dontrun{
#' birdnet_heatmap(data = birdnet_output, species = "Swainson's Thrush", threshold = 0.7)
#' }
#' @export
birdnet_heatmap <- function(data,
                            species,
                            threshold = NULL,
                            hour_range = c(0, 24),
                            min_date = NULL,
                            max_date = NULL) {
  # argument check ----------------------------------------------------------


  # Set default date range if not specified
  if (is.null(min_date)) min_date <- min(data, na.rm = TRUE)
  if (is.null(max_date)) max_date <- max(data, na.rm = TRUE)

  # main function -----------------------------------------------------------


  # add datetime and filter species
  data_with_time <- data |>
    birdnet_clean_names() |>
    birdnet_add_datetime()

  if (!is.null(threshold)) {
    data_after_filter <- data_with_time |>
      dplyr::filter(confidence >= threshold) |>
      dplyr::filter(common_name == species) |>
      dplyr::filter(date >= as.Date(min_date) & date <= as.Date(max_date)) |>
      dplyr::filter(hour >= hour_range[1] & hour <= hour_range[2])

  } else {
    data_after_filter <- data_with_time |>
      dplyr::filter(common_name == species) |>
      dplyr::filter(date >= as.Date(min_date) & date <= as.Date(max_date)) |>
      dplyr::filter(hour >= hour_range[1] & hour <= hour_range[2])
  }


  # generate the heatmap based on the specified type
  plot <- data_after_filter |>
    dplyr::summarise(activity = dplyr::n(), .by = c(date, hour)) |>
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(y = hour, x = date, fill = activity)) +
    ggplot2::scale_fill_viridis_c(option = "A", direction = -1) +
    ggplot2::labs(title = species, x = "Date", y = "Hour") +
    ggplot2::theme(
      legend.position = "none",
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(
        size = 14,
        margin = ggplot2::margin(t = 15, r = 15)
      )
    )

  return(plot)
}

