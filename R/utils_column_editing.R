#' Add datetime-related columns from BirdNET output filenames
#'
#' Extracts and parses datetime information from filenames in a specified column,
#' then adds several useful time-related columns (e.g., `date`, `year`, `month`, `hour`)
#' to the input data frame.
#'
#' @param data A data frame containing BirdNET output.
#' @param col Unquoted name of the column that contains the file paths (e.g., `File`).
#' @param tz Time zone to assign when parsing datetime. Defaults to `"UTC"`.
#'
#' @return A data frame with additional columns:
#' \describe{
#'   \item{datetime}{POSIXct datetime parsed from the filename.}
#'   \item{date}{Date portion of the datetime.}
#'   \item{year, month, mday, yday, hour, minute}{Individual datetime components.}
#' }
#'
#' @examples
#' \dontrun{
#' combined_data <- birdnet_combine("path/to/BirdNET/output")
#' data_with_time <- birdnet_add_datetime(combined_data, col = File)
#' }
#' @export
birdnet_add_datetime <- function(
    data,
    col = filepath,
    tz = "UTC") {
  # argument check ----------------------------------------------------------

  # if there is already all the columns, show the warning messages that the
  # original datetime will be kept and no new data will be calculated
  # if there is no column can be found to contain the path information, show error

  # main function -----------------------------------------------------------

  data_with_datetime <- data |>

    # parase the column name to the datetime format
    dplyr::mutate(datetime = {{ col }} |>
      basename() |>
      stringr::str_extract("\\d{8}.\\d{6}") |>
      lubridate::parse_date_time(orders = c("ymd_HMS", "ymd-HMS", "ymdHMS"),
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
#' @param data A data frame containing BirdNET output, with the datetime related columns present.
#'
#' @returns A data frame with datime related columns removed.
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







#' Clean and standardize column names
#'
#' This function standardizes column names in a BirdNET output-like dataframe.
#' It renames any columns that match patterns like "start", "end", "scientific",
#' "common", "file", or "confidence" (case-insensitive) to consistent names:
#' `"start"`, `"end"`, `"scientific_name"`, `"common_name"`, `"filepath"`, and `"confidence"`.
#'
#' @param data A data frame containing the output of BirdNET or similar data with timestamp and species information.
#'
#' @return A data frame with renamed columns.
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
#' @keywords internal
birdnet_clean_names <- function(data) {


  # argument check ----------------------------------------------------------


  # main function -----------------------------------------------------------

  data_with_cleaned_names <- data |>
    dplyr::rename_with(~ "start",
                       .cols = dplyr::matches("start", ignore.case = TRUE)) |>
    dplyr::rename_with(~ "end",
                       .cols = dplyr::matches("end", ignore.case = TRUE)) |>
    dplyr::rename_with(~ "scientific_name",
                       .cols = dplyr::matches("scientific", ignore.case = TRUE)) |>
    dplyr::rename_with(~ "common_name",
                       .cols = dplyr::matches("common", ignore.case = TRUE)) |>
    dplyr::rename_with(~ "filepath",
                       .cols = dplyr::matches("file", ignore.case = TRUE)) |>
    dplyr::rename_with(~ "confidence",
                       .cols = dplyr::matches("confidence", ignore.case = TRUE))

  return(data_with_cleaned_names)
}


