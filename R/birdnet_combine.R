#' Group BirdNET Output Files
#'
#' Reads and combines multiple BirdNET output CSV files from a specified directory into a single data frame.
#' Files named with "analysis_params" or "CombinedTable" under the specified directory are automatically excluded.
#'
#' @param path Character string. The directory path containing BirdNET output `.csv` files. Can include subdirectories.
#'
#' @return A data frame containing the combined BirdNET detection data from all valid CSV files, with columns:
#' \itemize{
#'   \item \code{Start (s)}: Detection start time in seconds.
#'   \item \code{End (s)}: Detection end time in seconds.
#'   \item \code{Scientific name}: Scientific name of the detected species.
#'   \item \code{Common name}: Common name of the detected species.
#'   \item \code{Confidence}: Confidence score of the detection.
#'   \item \code{File}: Name of the file where the detection was found.
#' }
#'
#' @details
#' This function is useful for aggregating BirdNET output from batch runs or large-scale deployments.
#' It uses a fixed column specification to ensure consistent parsing of each file. Files with incompatible
#' formats or errors are skipped and their names are printed with a warning.
#'
#' @examples
#' \dontrun{
#' combined_data <- group_BirdNET_output("path/to/BirdNET/output")
#' }
#'
#' @importFrom readr read_csv cols col_double col_character
#' @importFrom purrr map_dfr
#' @importFrom dplyr tibble
#' @importFrom cli cli_alert_success cli_alert_warning cli_li
#' @export
birdnet_combine <- function(path){

# argument check ----------------------------------------------------------


# main function -----------------------------------------------------------
  # define column types for read_csv
  column_spec <- readr::cols(
    `Start (s)` = readr::col_double(),
    `End (s)` = readr::col_double(),
    `Scientific name` = readr::col_character(),
    `Common name` = readr::col_character(),
    `Confidence` = readr::col_double(),
    `File` = readr::col_character()
  )


  # list all files in the directory
  all_files <- list.files(path,
                          pattern = "\\.csv$",
                          recursive = TRUE,
                          full.names = TRUE)

  filtered_files <- all_files[!grepl("analysis_params|CombinedTable", all_files)]


  # initialize a vector to store any files that cause errors
  error_files <- c()


  # use map_dfr with tryCatch to handle errors gracefully
  detections_raw <- filtered_files |>
    purrr::map_dfr(~ tryCatch({
      readr::read_csv(.x, col_types = column_spec)

    }, error = function(e) {
      # store the filename that caused the error
      error_files <<- c(error_files, .x)

      # return an empty tibble with the same column structure to continue
      dplyr::tibble(`Start (s)` = double(), `End (s)` = double(),
             `Scientific name` = character(), `Common name` = character(),
             `Confidence` = double(), `File` = character())

    }, warning = function(w) {
      # store the filename that caused the error
      error_files <<- c(error_files, .x)

      # return an empty tibble with the same column structure to continue
      dplyr::tibble(`Start (s)` = double(), `End (s)` = double(),
             `Scientific name` = character(), `Common name` = character(),
             `Confidence` = double(), `File` = character())
    }))


  # show messages about the number of files processed
  if (length(error_files) > 0) {
    cli::cli_alert_success("Combined {length(filtered_files) - length(error_files)} BirdNET output file{?s}.")

    cli::cli_alert_warning("The following {length(error_files)} file{?s} caused errors and were skipped:")
    cli::li(error_files)

  } else {
    cli::cli_alert_success("Combined all {length(filtered_files)} BirdNET output file{?s}.")
  }


  # return the combined data frame
  return(detections_raw)
}

