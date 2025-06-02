

birdnet_filter_species <- function(data, species){

  data <- data |>
    dplyr::filter(common_name %in% species)

  return(data)
}


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
      dplyr::filter(confidence >= .data$threshold) |>
      dplyr::select(-threshold)  # Optional: drop joined threshold column
  }

  return(data_filtered)
}


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


