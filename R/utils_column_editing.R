#' Add datetime-related columns from BirdNET output filenames
#'
#' Extracts and parses datetime information from filenames in the file path column
#' (automatically detected), then adds several useful time-related columns
#' (e.g., `date`, `year`, `month`, `hour`) to the input data frame. Several common
#' formats are tried by default, and users can supply their own with `formats`
#' (see Details).
#'
#'
#'
#' @param data A data frame containing BirdNET output.
#' @param formats A character vector of datetime formats, using the same codes
#'   as [strptime()] (see Details). Formats are tried in order, and the first
#'   one that successfully parses a file name is used. Defaults to common
#'   year-first formats, plus day-first and month-first variants. See Details
#'   for how ambiguous dates are handled.
#' @param tz A character string specifying the time zone to assign when parsing datetime.
#'   Defaults to `"UTC"`.
#' @param quiet Logical. If `TRUE`, suppresses the status messages about how
#'   many file names were parsed, failed, or were ambiguous. Warnings are still
#'   shown. Defaults to `FALSE`.
#'
#'
#' @details
#' **Defining your own formats.** If the default `formats` don't match your
#' file names, supply a format for each pattern in your data.
#'
#' Supported codes:
#' * `%Y`: 4-digit year (1900-2099)
#' * `%y`: 2-digit year
#' * `%m`: 2-digit month
#' * `%d`: 2-digit day
#' * `%H`, `%M`, `%S`: 2-digit hour, minute, second
#'
#' Any other characters (such as `_`, `-`, or `T`) are matched literally. For
#' example, `site1_2024-06-15_0530.wav` needs `formats = "%Y-%m-%d_%H%M"`.
#'
#' **Ambiguous dates.** The defaults include both day-first (`%d%m%Y`) and
#' month-first (`%m%d%Y`) formats. A file name such as `05062023_133500` is
#' valid as both, so the format listed first (day-first) is used and a message
#' lists the rows to check. If your data uses only one convention, supply that
#' format on its own to avoid the ambiguity.
#'
#'
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
#' Rows whose file names can't be parsed get `NA` in all of these columns.
#' Existing columns with these names are overwritten, with a warning. If all of
#' these columns already exist, the data is returned unchanged.
#'
#'
#'
#' @examples
#' \dontrun{
#' birdnet_add_datetime(example_jprf_2023) |>
#' glimpse()
#' }
#' @export
birdnet_add_datetime <- function(
    data,
    formats = c("%Y%m%dT%H%M%S",
                "%Y%m%d%H%M%S",

                "%Y%m%d_%H%M%S",
                "%Y%m%d_%H%M",

                "%d%m%Y_%H%M%S",
                "%m%d%Y_%H%M%S"),
    tz = "UTC",
    quiet = FALSE) {

  # argument check ----------------------------------------------------------

  checkmate::assert_data_frame(data)
  checkmate::assert_character(formats, min.len = 1, any.missing = FALSE)
  checkmate::assert_string(tz)
  checkmate::assert_flag(quiet)

  cols <- birdnet_detect_columns(data)

  # check if the required filepath column existing
  if (is.na(cols$filepath)) {
    cli::cli_abort("Can't find a column containing file names or paths.")
  }

  # check if the datetime columns already exist
  new_cols <- c("datetime", "date", "year", "month", "mday", "yday", "hour", "minute")
  if (all(new_cols %in% names(data))) {
    cli::cli_warn("Datetime columns already exist. Keeping the original and not recalculating.")
    return(data)
  }

  # check if some of the datetime columns already exist
  overlap <- intersect(new_cols, names(data))
  if (length(overlap) > 0) {
    cli::cli_warn("Existing column{?s} {.field {overlap}} will be overwritten.")
  }

  # main function -----------------------------------------------------------

  # get the file name to work with (which contains the datetime)
  fname <- stringr::str_extract(data[[cols$filepath]], "[^\\\\/]+$")

  # parase the column name to the datetime format
  parsed <- purrr::map(formats, \(f) {
    stringr::str_extract(fname, format_to_regex(f)) |>
      strptime(format = f, tz = tz) |>
      as.POSIXct(tz = tz)
  })

  dt <- Reduce(dplyr::coalesce, parsed)



  # ambiguity check - if one entry gets mapped into multiple formats, warn the user
  mat <- do.call(cbind, lapply(parsed, as.numeric))

  amb <- apply(mat, 1, \(r) length(unique(r[!is.na(r)]))) > 1
  amb_rows <- which(amb)
  n_ambiguous <- length(amb_rows)



  # tell the user exactly what went wrong instead of silently returning NAs
  n_total   <- length(dt)
  n_failed  <- sum(is.na(dt))
  n_success <- n_total - n_failed

  if (!quiet) {
    if (n_success > 0) {
      cli::cli_alert_success(
        "Parsed datetime for {n_success} of {n_total} file name{?s}."
      )
    }

    if (n_failed > 0) {
      cli::cli_alert_warning("{n_failed} of {n_total} file name{?s} could not be parsed into a datetime.")
      cli::cli_alert_info("Default {.arg formats} may not match your file names. See {.help birdnet_add_datetime} to define your own.")
    }

    if (n_ambiguous > 0) {
      cli::cli_alert_warning("{n_ambiguous} file name{?s} matched more than one format with different results. The first matching format was used.")
      shown <- utils::head(amb_rows, 5)
      more  <- if (n_ambiguous > 5) " (and more)" else ""
      cli::cli_alert_info("Check row{?s} {shown}{more}.")
    }
  }

  # mutate to add date and time components
  data_with_datetime <- data |>
    dplyr::mutate(
      datetime = dt,
      date = lubridate::date(datetime),
      year = lubridate::year(datetime),
      month = lubridate::month(datetime),
      mday = lubridate::mday(datetime),
      yday = lubridate::yday(datetime),
      hour = lubridate::hour(datetime),
      minute = lubridate::minute(datetime))

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
    match <- names(data)[stringr::str_detect(names(data), stringr::regex(pattern, ignore_case = TRUE))]
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

