#' Filter BirdNET output data by species, confidence, date, and time
#'
#' Applies one or more common filtering operations to a BirdNET output data frame.
#' Supports filtering by species name, confidence threshold (universal or species-specific),
#' year, date range, and hour of day.
#'
#' This function uses \code{\link{birdnet_detect_columns}} to automatically identify the relevant
#' columns (e.g., for species names, confidence, datetime) based on common naming patterns.
#'
#' All applied filter parameters are stored as an attribute called `"filter_log"` attached to
#' the returned data frame, which can be accessed via \code{attr(x, "filter_log")}.
#'
#' @param data A data frame containing BirdNET output. Relevant columns (e.g., common name,
#'   confidence, datetime) are automatically detected.
#' @param species Character vector. One or more common names of species to retain
#'   (e.g., \code{c("Swainson's Thrush", "American Robin")}).
#' @param threshold Either a single numeric value between 0 and 1 (for a universal threshold),
#'   or a data frame with columns `scientific_name` and `threshold` for species-specific thresholds.
#' @param year Integer or vector of integers specifying year(s) to retain (e.g., \code{2024:2025}).
#' @param min_date,max_date Optional. Character strings or \code{Date} objects specifying a date range
#'   in "YYYY-MM-DD" format. If only one is provided, filtering is open-ended on the other side.
#' @param hour Integer vector between 0 and 23 specifying hours of the day to retain (e.g., \code{4:7}).
#'
#' @return A filtered data frame with an attribute `"filter_log"` containing the applied filters.
#'
#' @examples
#' \dontrun{
#' filtered <- birdnet_filter(
#'   data = birdnet_combine("path/to/output"),
#'   species = "Swainson's Thrush",
#'   threshold = 0.75,
#'   year = 2025,
#'   min_date = "2025-06-01",
#'   hour = 4:8
#' )
#' attr(filtered, "filter_log")
#' }
#' @export
birdnet_filter <- function(
    data,
    species = NULL,
    threshold = NULL,
    year = NULL,
    min_date = NULL,
    max_date = NULL,
    hour = NULL
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

  # 2. species: NULL or character vector
  if (!is.null(species)) {
    checkmate::assert_character(species, any.missing = FALSE, min.len = 1)
  }

  # 3. threshold: NULL or numeric scalar/vector or tibble with "species" and "threshold" columns
  if (!is.null(threshold)) {
    if (inherits(threshold, "tbl") || inherits(threshold, "data.frame")) {
      checkmate::assert_names(names(threshold), must.include = c("scientific_name", "threshold"))
      checkmate::assert_character(threshold$scientific_name, any.missing = FALSE)
      checkmate::assert_numeric(threshold$threshold, lower = 0, upper = 1, any.missing = FALSE)
    } else {
      # numeric scalar or vector with values between 0 and 1
      checkmate::assert_numeric(threshold, lower = 0, upper = 1, any.missing = FALSE)
    }
  }

  # 4. year: NULL or numeric vector (assuming it should be numeric years)
  if (!is.null(year)) {
    checkmate::assert_numeric(year, any.missing = FALSE)
  }

  # 5. min_date and max_date: NULL or character strings in "YYYY-MM-DD" format
  date_pattern <- "^\\d{4}-\\d{2}-\\d{2}$"
  if (!is.null(min_date)) {
    checkmate::assert_character(min_date, len = 1, any.missing = FALSE)
    if (!grepl(date_pattern, min_date)) {
      rlang::abort("`min_date` must be a character string in 'YYYY-MM-DD' format.")
    }
  }
  if (!is.null(max_date)) {
    checkmate::assert_character(max_date, len = 1, any.missing = FALSE)
    if (!grepl(date_pattern, max_date)) {
      rlang::abort("`max_date` must be a character string in 'YYYY-MM-DD' format.")
    }
  }

  # 6. hour: NULL or numeric vector with values between 0 and 23 inclusive
  if (!is.null(hour)) {
    checkmate::assert_numeric(hour, lower = 0, upper = 23, any.missing = FALSE)
  }



  # main function -----------------------------------------------------------
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
