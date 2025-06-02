#' Filter BirdNET output data by species, confidence, date, and time
#'
#' Applies one or more common filtering operations to a BirdNET output dataframe.
#' This includes filtering by species name, confidence threshold, year, date range, and hour range.
#' The function also records the filtering parameters as an attribute (`filter_log`)
#' attached to the returned dataframe, allowing users to inspect what filters were applied.
#'
#' @param data A data frame containing BirdNET output. Column names will be standardized internally
#'   using [birdnet_clean_names()].
#' @param species Character vector. One or more common names of the species to filter by (e.g., `c("Swainson's Thrush", "American Robin")`).
#' @param threshold Numeric. Confidence threshold (between 0 and 1); rows with confidence below
#'   this value will be excluded.
#' @param year Integer or vector of integers. Year(s) to retain in the data (e.g., `c(2024:2025)`).
#' @param min_date,max_date Character strings or `Date` objects specifying the date range (inclusive).
#'   If only one is provided, filtering is open-ended on the other side (e.g., "2025-06-09").
#' @param hour Integer vector between 0 and 23 specifying which hours of the day to keep (e.g., `c(4:7)`).
#'
#' @return A filtered data frame with a `"filter_log"` attribute containing the filtering parameters used.
#'
#' @examples
#' \dontrun{
#' filtered <- birdnet_filter(data,
#'   species = "Swainson's Thrush",
#'   threshold = 0.75
#' )
#' attr(filtered, "filter_log")
#' }
#'
#' @export

birdnet_filter <- function(data,
                           species = NULL,
                           threshold = NULL,
                           year = NULL,
                           min_date = NULL,
                           max_date = NULL,
                           hour = NULL) {
  # clean column names first
  data <- birdnet_clean_names(data)

  # initialize filter log
  filter_log <- list()

  # start with the full dataset
  data_filtered <- data

  # filter by species
  if (!is.null(species)) {
    data_filtered <- birdnet_filter_species(data_filtered, species)
    filter_log$species <- species
  }

  # filter by threshold
  if (!is.null(threshold)) {
    data_filtered <- birdnet_filter_threshold(data_filtered, threshold)
    filter_log$threshold <- threshold
  }

  # filter by year
  if (!is.null(year)) {
    data_filtered <- birdnet_filter_year(data_filtered, year)
    filter_log$year <- year
  }

  # filter by date range
  if (!is.null(min_date) || !is.null(max_date)) {
    data_filtered <- birdnet_filter_date_range(data_filtered, min_date, max_date)
    filter_log$min_date <- min_date
    filter_log$max_date <- max_date
  }

  # filter by hour
  if (!is.null(hour)) {
    data_filtered <- birdnet_filter_hour(data_filtered, hour)
    filter_log$hour <- hour
  }

  # attach filtering metadata
  attr(data_filtered, "filter_log") <- filter_log

  return(data_filtered)
}
