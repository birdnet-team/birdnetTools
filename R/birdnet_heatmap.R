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
#' @param hour Integer vector between 0 and 23 specifying which hours of the day to keep (e.g., `c(4:7)`).
#' @param min_date Character or `NULL`. The minimum date to include in the heatmap. If `NULL`, the earliest date will be used.
#' @param max_date Character or `NULL`. The maximum date to include in the heatmap. If `NULL`, the latest date will be used.
#'
#' @return A `ggplot` heatmap showing the number of detections by hour and date.
#' @examples
#' \dontrun{
#' birdnet_heatmap(data = birdnet_output, species = "Swainson's Thrush", threshold = 0.7)
#' }
#' @export
birdnet_heatmap <- function(data,
                            species = NULL,
                            threshold = NULL,
                            min_date = NULL,
                            max_date = NULL,
                            hour = NULL) {

  # argument check ----------------------------------------------------------

  # 1. Check data is a tibble with required columns
  checkmate::assert_data_frame(data)

  required_cols <- c("filepath", "start", "end", "common_name", "scientific_name", "confidence")
  missing_cols <- setdiff(required_cols, names(data |> birdnet_clean_names()))
  if (length(missing_cols) > 0) {
    rlang::abort(
      message = paste0("Input `data` is missing required columns: ", paste(missing_cols, collapse = ", "))
    )
  }

  # 2. species: NULL or character vector with no missing
  if (!is.null(species)) {
    checkmate::assert_character(species, any.missing = FALSE, min.len = 1)
  }

  # 3. threshold: NULL or numeric scalar/vector between 0 and 1 OR
  # dataframe with columns "scientific_name" and "threshold"
  if (!is.null(threshold)) {
    if (inherits(threshold, "tbl") || inherits(threshold, "data.frame")) {
      checkmate::assert_names(names(threshold), must.include = c("scientific_name", "threshold"))
      checkmate::assert_character(threshold$scientific_name, any.missing = FALSE)
      checkmate::assert_numeric(threshold$threshold, lower = 0, upper = 1, any.missing = FALSE)
    } else {
      checkmate::assert_numeric(threshold, lower = 0, upper = 1, any.missing = FALSE)
      checkmate::assert_true(length(threshold) == 1)
    }
  }

  # 4. min_date and max_date: NULL or character strings (assumed date format checked downstream)
  if (!is.null(min_date)) {
    checkmate::assert_character(min_date, len = 1, any.missing = FALSE)
  }
  if (!is.null(max_date)) {
    checkmate::assert_character(max_date, len = 1, any.missing = FALSE)
  }

  # 5. hour: NULL or numeric vector with values between 0 and 23 inclusive
  if (!is.null(hour)) {
    checkmate::assert_numeric(hour, lower = 0, upper = 23, any.missing = FALSE)
  }



  # main function -----------------------------------------------------------
  # clean names and add datetime
  data_with_time <- data |>
    birdnet_clean_names() |>
    birdnet_add_datetime()


  # filter data based on date and hour range
  data_after_filter <- data_with_time |>
    birdnet_filter(
      species = species,
      threshold = threshold,
      min_date = min_date,
      max_date = max_date,
      hour = hour)


  # generate the heatmap based on the specified type
  plot <- data_after_filter |>
    dplyr::summarise(activity = dplyr::n(), .by = c(date, hour)) |>
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(y = hour, x = date, fill = activity)) +
    ggplot2::scale_fill_viridis_c(option = "A", direction = -1) +
    ggplot2::labs(x = "Date", y = "Hour") +
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

