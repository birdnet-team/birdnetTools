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
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' combined_data <- birdnet_combine("path/to/BirdNET/output")
#' data_with_time <- birdnet_add_datetime(combined_data, col = File)
#' }

birdnet_add_datetime <- function(data, col = File, tz = "UTC") {
  # argument check ----------------------------------------------------------


  # main function -----------------------------------------------------------

  data_add_datetime <- data |>
    dplyr::mutate(datetime = {{ col }} |>
      #normalizePath(winslash = "/") |>
      basename() |>
      stringr::str_extract("\\d{8}.\\d{6}") |>
      lubridate::parse_date_time(orders = c("ymd_HMS", "ymd-HMS", "ymdHMS"),
                                 tz = tz)) |>
    dplyr::mutate(
      date = lubridate::date(datetime),
      year = lubridate::year(datetime),
      month = lubridate::month(datetime),
      mday = lubridate::mday(datetime),
      yday = lubridate::yday(datetime),
      hour = lubridate::hour(datetime),
      minute = lubridate::minute(datetime)
    )

  return(data_add_datetime)
}





clean_names <- function(data) {


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
    dplyr::rename_with(~ "file",
                       .cols = dplyr::matches("file", ignore.case = TRUE)) |>
    dplyr::rename_with(~ "confidence",
                       .cols = dplyr::matches("confidence", ignore.case = TRUE))

  return(data_with_cleaned_names)
}









