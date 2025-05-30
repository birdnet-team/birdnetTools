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
birdnet_add_datetime <- function(data, col = filepath, tz = "UTC") {
  # argument check ----------------------------------------------------------


  # main function -----------------------------------------------------------

  data_add_datetime <- data |>

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

  return(data_add_datetime)
}
