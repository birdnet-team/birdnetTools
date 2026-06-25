#' Add datetime-related columns from BirdNET output filenames
#'
#' Extracts and parses datetime information from filenames in the file path column
#' (automatically detected), then adds several useful time-related columns
#' (e.g., `date`, `year`, `month`, `hour`) to the input data frame.
#'
#' The function uses [birdnet_detect_columns] to find the column
#' containing file paths based on common name patterns (e.g., "file", "path").
#' Filenames are expected to contain a datetime string in the format
#' `"YYYYMMDD_HHMMSS"` or similar.
#'
#' @param data A data frame containing BirdNET output.
#' @param tz A character string specifying the time zone to assign when parsing datetime.
#'   Defaults to `"UTC"`.
#'
#' @return A data frame with additional columns:
#' \describe{
#'   \item{datetime}{POSIXct datetime parsed from the filename.}
#'   \item{date}{Date portion of the datetime.}
#'   \item{year}{Year of detection.}
#'   \item{month}{Month of detection.}
#'   \item{mday}{Day of the month.}
#'   \item{yday}{Day of the year.}
#'   \item{hour}{Hour of the day.}
#'   \item{minute}{Minute of the hour.}
#' }
#'
#' @examples
#' \dontrun{
#' combined_data <- birdnet_combine("path/to/BirdNET/output")
#' data_with_time <- birdnet_add_datetime(combined_data)
#' }
#'
#' @export
birdnet_add_datetime <- function(
    data,
    #col = filepath,
    tz = "UTC") {
  # argument check ----------------------------------------------------------

  # if there is already all the columns, show the warning messages that the
  # original datetime will be kept and no new data will be calculated
  # if there is no column can be found to contain the path information, show error

  # main function -----------------------------------------------------------

  cols <- birdnet_detect_columns(data)


  data_with_datetime <- data |>

    # parase the column name to the datetime format
    dplyr::mutate(
      datetime = basename(.data[[cols$filepath]]) |>
        stringr::str_extract("\\d{8}.\\d{6}") |>
        lubridate::parse_date_time(
          orders = c("ymd_HMS", "ymd-HMS", "ymdHMS", "ymd HM"),
          tz = tz)) |>

    # mutate to add date and time components
    dplyr::mutate(
      date = lubridate::date(datetime),
      year = lubridate::year(datetime),
      month = lubridate::month(datetime),
      mday = lubridate::mday(datetime),
      yday = lubridate::yday(datetime),
      hour = lubridate::hour(datetime),
      minute = lubridate::minute(datetime)
    )

  return(data_with_datetime)
}




#' Drop datetime-related columns from BirdNET output
#'
#' Removes datetime-related columns (e.g., `datetime`, `date`, `year`, `hour`, etc.)
#' that may have been added using [birdnet_add_datetime].
#'
#' @param data A data frame containing BirdNET output, with datetime-related columns present.
#'
#' @return A data frame with datetime-related columns removed, if present.
#' @keywords internal
birdnet_drop_datetime <- function(data) {
  # Define expected datetime-related columns
  datetime_cols <- c("datetime", "date", "year", "month",
                     "mday", "yday", "hour", "minute")

  # Keep only columns that exist in the data
  cols_to_drop <- intersect(datetime_cols, colnames(data))

  # Drop them (if any)
  data <- data |>
    dplyr::select(-dplyr::all_of(cols_to_drop))

  return(data)
}





#' Add site column from BirdNET output filenames
#'
#' Extracts a specific directory level from the file path column (automatically
#' detected) to act as a site identifier, then adds a `site` column to the
#' input data frame.
#'
#' The function uses [birdnet_detect_columns] to find the column containing
#' file paths based on common name patterns. By default, it looks at the
#' immediate parent folder of the file.
#'
#' @param data A data frame containing BirdNET output.
#' @param i An integer specifying the index of the path element to extract
#'   when split by slashes. Defaults to `-2`, which corresponds to the immediate
#'   parent directory of the file (e.g., extracting "Site-A" from
#'   "path/to/Site-A/audio.wav"). Negative values count from the right-hand side.
#'
#' @return A data frame with an additional column:
#' \describe{
#'   \item{site}{The extracted directory or folder name used as the site identifier.}
#' }
#'
#' @examples
#' \dontrun{
#' combined_data <- birdnet_combine("path/to/BirdNET/output")
#' data_with_site <- birdnet_add_site(combined_data, i = -2)
#' }
#'
#' @keywords internal
birdnet_add_site <- function(data,
                             i = -2) {

  # argument check ----------------------------------------------------------

  # detect columns
  cols <- birdnet_detect_columns(data)


  # ensure filepath column was actually found
  if (is.null(cols$filepath) || !(cols$filepath %in% colnames(data))) {
    stop("Could not automatically detect a valid file path column in the data.")
  }

  # main function -----------------------------------------------------------

  # extract site safely using tidy evaluation data masking
  data_with_site <- data |>
    dplyr::mutate(
      site = stringr::str_split_i(.data[[cols$filepath]], "[/\\\\]", i = i)
    )

  return(data_with_site)
}









#' Clean and standardize column names
#'
#' @description
#' **DEPRECATED.** This function is no longer recommended for use. Consider using
#' [birdnet_detect_columns] instead for robust column detection and standardization.
#'
#' Standardizes column names in a BirdNET-like data frame. This function renames columns
#' matching patterns like "start", "end", "scientific", "common", "file", or "confidence"
#' (case-insensitive) to consistent names:
#' `"start"`, `"end"`, `"scientific_name"`, `"common_name"`, `"filepath"`, and `"confidence"`.
#'
#' @param data A data frame containing BirdNET output or similar data with timestamp and species information.
#'
#' @return A data frame with renamed, standardized column names.
#'
#' @seealso [birdnet_detect_columns]
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   Start.Time = 1:3,
#'   End.Time = 4:6,
#'   Scientific = c("Turdus migratorius", "Cyanocitta cristata", "Corvus brachyrhynchos"),
#'   Common = c("American Robin", "Blue Jay", "American Crow"),
#'   File.Name = c("file1.wav", "file2.wav", "file3.wav"),
#'   Confidence.Score = c(0.95, 0.87, 0.90)
#' )
#' birdnet_clean_names(df)
#' }
#'
#' @keywords internal
birdnet_clean_names <- function(data) {

  rename_if_single <- function(df, pattern, new_name) {
    cols <- grep(pattern, names(df), ignore.case = TRUE, value = TRUE)
    if (length(cols) == 1) {
      df <- dplyr::rename(df, !!new_name := !!cols)
    }
    df
  }

  data %>%
    rename_if_single("start", "start") %>%
    rename_if_single("end", "end") %>%
    rename_if_single("scientific", "scientific_name") %>%
    rename_if_single("common", "common_name") %>%
    rename_if_single("file", "filepath") %>%
    rename_if_single("confidence", "confidence")
}





#' Detect standard BirdNET column names in a data frame
#'
#' Helper function to identify the most likely columns representing
#' key BirdNET output variables such as start time, end time,
#' scientific name, common name, confidence, and filepath.
#'
#' This function attempts to match user-supplied data frame column
#' names to expected BirdNET output columns by pattern matching.
#' It returns a named list with the detected column names or `NA`
#' if no match is found.
#'
#' @param data A data frame containing BirdNET output data.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{start}{Column name for start time (or NA).}
#'   \item{end}{Column name for end time (or NA).}
#'   \item{scientific_name}{Column name for scientific name (or NA).}
#'   \item{common_name}{Column name for common name (or NA).}
#'   \item{confidence}{Column name for confidence score (or NA).}
#'   \item{filepath}{Column name for file path or file name (or NA).}
#' }
#'
#' @keywords internal
birdnet_detect_columns <- function(data) {
  checkmate::assert_data_frame(data)

  col_matches <- function(patterns) {
    pattern <- stringr::str_c(patterns, collapse = "|")  # create a single regex pattern
    match <- names(data)[stringr::str_detect(names(data),
                                             stringr::regex(pattern, ignore_case = TRUE))]
    if (length(match) == 0) return(NA_character_)
    return(match[1])  # return first match
  }

  list(
    start = col_matches(c("start", "begin time")),
    end = col_matches("end"),
    scientific_name = col_matches("scientific"),
    common_name = col_matches(c("common", "label")),
    confidence = col_matches(c("confidence", "score")),
    filepath = col_matches(c("file", "path"))
  )
}

