#' Filter BirdNET data by species
#'
#' Filters a BirdNET output data frame for one or more specified species using the
#' column automatically detected as containing common species names.
#'
#' This function uses [birdnet_detect_columns] to identify the
#' appropriate column (e.g., "Common Name", "common_name", etc.) containing
#' common names of species, then filters the data frame to retain only the rows
#' matching the specified species.
#'
#' @param data A data frame containing BirdNET output.
#' @param species Character vector. One or more common names of species to keep.
#'
#' @return A data frame filtered to include only the specified species.
#'
#' @seealso [birdnet_detect_columns]
#' @keywords internal
birdnet_filter_species <- function(data, species){

  cols <- birdnet_detect_columns(data)

  data <- data |>
    dplyr::filter(!!dplyr::sym(cols$common_name) %in% species)

  return(data)
}




#' Filter BirdNET data by confidence threshold
#'
#' Filters a BirdNET output data frame based on a confidence threshold, using either
#' a universal numeric value or species-specific thresholds.
#'
#' The function uses [birdnet_detect_columns] to automatically detect
#' column names (e.g., "Confidence", "Scientific Name") based on common patterns.
#'
#' @param data A data frame containing BirdNET output.
#' @param threshold_arg Either:
#'   \itemize{
#'     \item A single numeric value specifying a universal confidence threshold, or
#'     \item A data frame with columns `scientific_name` and `threshold`,
#'     specifying species-specific thresholds.
#'   }
#'
#' @return A data frame filtered to include only rows where confidence scores meet
#' the threshold criteria.
#'
#' @seealso [birdnet_detect_columns]
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


#' Filter BirdNET data by year
#'
#' Filters a BirdNET output data frame based on one or more specified years of detection.
#' If the `year` column is missing, the function will extract it from available datetime columns.
#'
#' @param data A data frame containing BirdNET output. If the `year` column is absent,
#' the function will attempt to extract datetime information using [birdnet_add_datetime].
#' @param year_arg Integer vector. The year or years to retain in the filtered data.
#'
#' @return A filtered data frame containing only rows with detections from the specified years.
#'
#' @seealso [birdnet_add_datetime], [birdnet_drop_datetime]
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




#' Filter BirdNET data by date range
#'
#' Filters a BirdNET output data frame to include only rows with detections within
#' a specified date range. If the `date` column is missing, it will be extracted
#' from available datetime columns.
#'
#' @param data A data frame containing BirdNET output. If the `date` column is not present,
#' the function will attempt to extract it using `birdnet_add_datetime`.
#' @param min_date Date or character string coercible to Date. The earliest date to include (inclusive).
#' @param max_date Date or character string coercible to Date. The latest date to include (inclusive).
#'
#' @return A data frame filtered to include only rows within the specified date range.
#'
#' @seealso `birdnet_add_datetime`, `birdnet_drop_datetime`
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




#' Filter BirdNET data by hour of day
#'
#' Filters a BirdNET output data frame to include only rows with detections
#' occurring during specified hours of the day (0–23). If the `hour` column
#' is missing, it will be extracted from available datetime columns.
#'
#' @param data A data frame containing BirdNET output. If the `hour` column is not present,
#' the function will attempt to extract it using [birdnet_add_datetime].
#' @param hour_arg Integer vector of hours (from 0 to 23) to retain.
#'
#' @return A data frame filtered to include only detections within the specified hours.
#'
#' @seealso [birdnet_add_datetime], [birdnet_drop_datetime]
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


