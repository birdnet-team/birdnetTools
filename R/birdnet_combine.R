#' Group BirdNET output files
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
birdnet_combine <- function(path) {
  # argument check ----------------------------------------------------------


  # main function -----------------------------------------------------------


  # list all files in the directory
  all_files <- list.files(
    path,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )


  # filter out unwanted files
  filtered_files <- all_files[!grepl("analysis_params|CombinedTable",
                                     all_files)]


  # initialize a vector to store any files that cause errors
  error_files <- c()


  # use map_dfr with tryCatch to handle errors gracefully
  detections_raw <- filtered_files |>
    purrr::map_dfr(~ tryCatch(
      {
        .x |> readr::read_csv(show_col_types = FALSE) |>
          birdnet_clean_names() |>
          dplyr::mutate(
            start = as.numeric(start),
            end = as.numeric(end),
            scientific_name = as.character(scientific_name),
            common_name = as.character(common_name),
            confidence = as.numeric(confidence),
            filepath = as.character(filepath)
          )

      }, error = function(e) {
        # store the filename that caused the error
        error_files <<- c(error_files, .x)

        # return an empty tibble with the same column structure to continue
        dplyr::tibble(
          start = numeric(0),          # numeric for doubles
          end = numeric(0),
          scientific_name = character(0),
          common_name = character(0),
          confidence = numeric(0),
          filepath = character(0)
        )

      }, warning = function(w) {
        # store the filename that caused the error
        error_files <<- c(error_files, .x)

        # return an empty tibble with the same column structure to continue
        dplyr::tibble(
          start = numeric(0),          # numeric for doubles
          end = numeric(0),
          scientific_name = character(0),
          common_name = character(0),
          confidence = numeric(0),
          filepath = character(0)
        )
      }
    ))


  # show messages about the number of files processed
  if (length(error_files) > 0) {
    cli::cli_alert_success("Successfully combined {length(filtered_files) - length(error_files)} BirdNET output file{?s}")
    cli::cli_alert_warning("The following {length(error_files)} file{?s} caused errors and were skipped:")
    cli::cli_li(error_files)
  } else {
    cli::cli_alert_success("Successfully combined {length(filtered_files)} BirdNET output file{?s}")
  }


  # return the combined data frame
  return(detections_raw)
}
