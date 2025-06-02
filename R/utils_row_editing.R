#' Filter data by species
#'
#' Filters BirdNET data for one or more specified species.
#'
#' @param data A data frame containing BirdNET output with a `common_name` column.
#' @param species Character vector. One or more common names of species to keep.
#'
#' @return A filtered data frame.
#' @noRd
birdnet_filter_species <- function(data, species){

  data <- data |>
    dplyr::filter(common_name %in% species)

  return(data)
}


#' Filter data by confidence threshold
#'
#' Filters BirdNET data based on a universal or species-specific confidence threshold.
#'
#' @param data A data frame containing BirdNET output with `confidence` and `scientific_name` columns.
#' @param threshold_arg Either a single numeric value (for universal threshold) or a data frame
#'   with `scientific_name` and `threshold` columns for species-specific thresholds.
#'
#' @return A filtered data frame.
#' @noRd
birdnet_filter_threshold <- function(data, threshold_arg) {

  # check the threshold is either a single numeric value or
  # a dataframe with `scientific_name' and 'threshold' columns

  if (is.numeric(threshold_arg)) {
    # universal threshold
    data_filtered <- dplyr::filter(data, confidence >= threshold_arg)

  } else if (is.data.frame(threshold_arg)) {
    # species-specific thresholds
    data_filtered <- data |>
      dplyr::left_join(threshold_arg, by = "scientific_name") |>
      dplyr::filter(confidence >= threshold) |>
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
#' @noRd
birdnet_filter_year <- function(data, year_arg) {


  # add datetime info if 'year' column is missing
  if (!"year" %in% colnames(data)) {
    data <- birdnet_add_datetime(data)
    has_datetime <- TRUE
  } else {
    has_datetime <- FALSE
  }

  # filter by year
  data <- dplyr::filter(data, year %in% year_arg)

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
#' @noRd
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
    data <- dplyr::filter(data, date >= as.Date(min_date))
  }

  # filter by maximum date
  if (!is.null(max_date)) {
    data <- dplyr::filter(data, date <= as.Date(max_date))
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
#' @noRd
birdnet_filter_hour <- function(data, hour_arg) {

  # add datetime info if 'hour' column is missing
  if (!"hour" %in% colnames(data)) {
    data <- birdnet_add_datetime(data)
    has_datetime <- TRUE
  } else {
    has_datetime <- FALSE
  }

  # filter by year
  data <- dplyr::filter(data, hour %in% hour_arg)

  # drop datetime if it was added
  if (has_datetime) {
    data <- birdnet_drop_datetime(data)
  }

  return(data)
}


