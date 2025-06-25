#' Filter data by species
#'
#' Filters BirdNET data for one or more specified species using the column
#' automatically detected as containing common species names.
#'
#' The function uses \code{\link{birdnet_detect_columns}} to detect the column
#' containing common names (e.g., "Common Name", "common_name", etc.).
#'
#' @param data A data frame containing BirdNET output.
#' @param species Character vector. One or more common names of species to keep.
#'
#' @return A filtered data frame containing only the specified species.
#' @keywords internal
birdnet_filter_species <- function(data, species){

  cols <- birdnet_detect_columns(data)

  data <- data |>
    dplyr::filter(!!dplyr::sym(cols$common_name) %in% species)

  return(data)
}


#' Filter data by confidence threshold
#'
#' Filters BirdNET data based on a universal or species-specific confidence threshold.
#' Column names (e.g., for confidence and scientific name) are detected automatically.
#'
#' The function uses \code{\link{birdnet_detect_columns}} to detect the relevant columns
#' (e.g., "Confidence", "Scientific Name") based on common patterns.
#'
#' @param data A data frame containing BirdNET output.
#' @param threshold_arg Either a single numeric value (for a universal threshold), or a data frame
#'   with columns `scientific_name` and `threshold` specifying species-specific thresholds.
#'
#' @return A filtered data frame with only rows meeting the threshold criteria.
#' @keywords internal
birdnet_filter_threshold <- function(data, threshold_arg) {

  cols <- birdnet_detect_columns(data)

  # check the threshold is either a single numeric value or
  # a dataframe with `scientific_name' and 'threshold' columns

  if (is.numeric(threshold_arg)) {
    # universal threshold
    data_filtered <- data |>
      dplyr::filter(!!dplyr::sym(cols$confidence) >= threshold_arg)

  } else if (is.data.frame(threshold_arg)) {
    # species-specific thresholds
    data_filtered <- data |>
      dplyr::left_join(threshold_arg,
                       by = dplyr::join_by(!!dplyr::sym(cols$scientific_name) == "scientific_name")) |>
      dplyr::filter(!!dplyr::sym(cols$confidence) >= threshold) |>
      dplyr::select(-threshold)  # Optional: drop joined threshold column
  }

  return(data_filtered)
}


#' Filter data by year
#'
#' Filters BirdNET data based on the year of detection.
#'
#' @param data A data frame with (or without) a `year` column. If missing, datetime columns will be added.
#' @param year_arg Integer vector. The year(s) to keep.
#'
#' @return A filtered data frame.
#' @keywords internal
birdnet_filter_year <- function(data, year_arg) {


  # add datetime info if 'year' column is missing
  if (!"year" %in% colnames(data)) {
    data <- birdnet_add_datetime(data)
    has_datetime <- TRUE
  } else {
    has_datetime <- FALSE
  }


  # filter by year
  data <- data |>
    dplyr::filter(year %in% year_arg)

  # drop datetime if it was added
  if (has_datetime) {
    data <- birdnet_drop_datetime(data)
  }

  return(data)
}


#' Filter data by date range
#'
#' Filters BirdNET data to only include rows within a specified date range.
#'
#' @param data A data frame with (or without) a `date` column. If missing, datetime columns will be added.
#' @param min_date Date or character string coercible to Date. The minimum date (inclusive).
#' @param max_date Date or character string coercible to Date. The maximum date (inclusive).
#'
#' @return A filtered data frame.
#' @keywords internal
birdnet_filter_date_range <- function(data,
                                      min_date = NULL,
                                      max_date = NULL) {

  # add datetime info if 'date' column is missing
  if (!"date" %in% colnames(data)) {
    data <- birdnet_add_datetime(data)
    has_datetime <- TRUE
  } else {
    has_datetime <- FALSE
  }

  # filter by minimum date
  if (!is.null(min_date)) {
    data <- data |>
      dplyr::filter(date >= as.Date(min_date))
  }

  # filter by maximum date
  if (!is.null(max_date)) {
    data <- data |>
      dplyr::filter(date <= as.Date(max_date))
  }

  # drop datetime if it was added
  if (has_datetime) {
    data <- birdnet_drop_datetime(data)
  }

  return(data)
}


#' Filter data by hour
#'
#' Filters BirdNET data to only include rows from specific hours of the day.
#'
#' @param data A data frame with (or without) an `hour` column. If missing, datetime columns will be added.
#' @param hour_arg Integer vector of hours (0–23) to keep.
#'
#' @return A filtered data frame.
#' @keywords internal
birdnet_filter_hour <- function(data, hour_arg) {

  # add datetime info if 'hour' column is missing
  if (!"hour" %in% colnames(data)) {
    data <- birdnet_add_datetime(data)
    has_datetime <- TRUE
  } else {
    has_datetime <- FALSE
  }

  # filter by year
  data <- data |>
    dplyr::filter(hour %in% hour_arg)

  # drop datetime if it was added
  if (has_datetime) {
    data <- birdnet_drop_datetime(data)
  }

  return(data)
}


