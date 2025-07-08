#' Create a heatmap of BirdNET detections by hour and date
#'
#' Generates a heatmap visualizing the daily activity pattern of a specified species
#' detected in BirdNET output data. The heatmap shows detection counts by hour and date,
#' optionally filtered by species, confidence threshold, date range, and hours of the day.
#'
#' @param data A data frame containing BirdNET output. Relevant columns (e.g., `common name`,
#'   `confidence`, `datetime`, `filepath`, etc.) are automatically detected by
#'   [birdnet_detect_columns]. Must include columns like `common_name`, `confidence`, and `datetime`.
#' @param species Character scalar or vector specifying the common name(s) of species to visualize.
#'   If `NULL`, no species filtering is applied.
#' @param threshold Either a numeric scalar between 0 and 1 (applied uniformly), or a data frame
#'   with columns `common_name` and `threshold` for species-specific values. If `NULL`, no
#'   threshold filtering is applied.
#' @param min_date Optional character scalar giving the earliest date to include (`"YYYY-MM-DD"` format).
#' @param max_date Optional character scalar giving the latest date to include (`"YYYY-MM-DD"` format).
#' @param hour Optional integer vector (0–23) specifying hours of the day to include in the heatmap.
#'
#' @return A `ggplot` object showing a heatmap of detections by date (x-axis) and hour (y-axis).
#'   The fill color corresponds to detection counts.
#'
#' @examples
#' \dontrun{
#' birdnet_heatmap(
#'   data = birdnet_output,
#'   species = "Swainson's Thrush",
#'   threshold = 0.7,
#'   min_date = "2024-06-01",
#'   max_date = "2024-06-30",
#'   hour = 4:7
#' )
#' }
#' @export
birdnet_heatmap <- function(data, species = NULL, threshold = NULL, min_date = NULL, max_date = NULL, hour = NULL
) {

  # argument check ----------------------------------------------------------

  # 1. Check data is a tibble with required columns
  checkmate::assert_data_frame(data)

  cols <- birdnet_detect_columns(data)

  required_cols <- c("start", "end", "scientific_name", "common_name", "confidence", "filepath")

  missing_cols <- required_cols[is.na(cols)]

  if (length(missing_cols) > 0) {
    rlang::abort(
      paste0(
        "The input data is missing required BirdNET columns: ",
        paste(missing_cols, collapse = ", "),
        ". Please provide a valid BirdNET output data frame."
      )
    )
  }

  # 2. species: NULL or character vector with no missing
  if (!is.null(species)) {
    checkmate::assert_character(species, any.missing = FALSE, min.len = 1)
  }

  # 3. threshold: NULL or numeric scalar/vector between 0 and 1 OR
  # dataframe with columns "common_name" and "threshold"
  if (!is.null(threshold)) {
    if (inherits(threshold, "tbl") || inherits(threshold, "data.frame")) {
      checkmate::assert_names(names(threshold), must.include = c("common_name", "threshold"))
      checkmate::assert_character(threshold$common_name, any.missing = FALSE)
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
  # add datetime
  data_with_time <- data |>
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
    ggplot2::labs(x = "Date", y = "Hour", fill = "Detections") +
    ggplot2::theme(
      legend.key.height = ggplot2::unit(1.5, "cm"),
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(
        size = 14,
        margin = ggplot2::margin(t = 15, r = 15)
      )
    )

  return(plot)
}

