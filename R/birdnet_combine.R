#' Group BirdNET output files
#'
#' Reads and combines multiple BirdNET output .csv or .txt files from a specified directory
#' into a single data frame.
#' Files named with "analysis_params" or "CombinedTable" under the specified directory are automatically excluded.
#'
#' @param path Character string. The directory path containing BirdNET output `.csv` or `.txt` files. Can include subdirectories.
#'
#' @return A data frame containing the combined BirdNET detection data from all valid files, with columns:
#' \itemize{
#'   \item \code{start}: Detection start time in seconds.
#'   \item \code{end}: Detection end time in seconds.
#'   \item \code{scientific_name}: Scientific name of the detected species.
#'   \item \code{common_name}: Common name of the detected species.
#'   \item \code{confidence}: Confidence score of the detection.
#'   \item \code{filepath}: Name of the file where the detection was found.
#' }
#'
#' @details
#' This function is useful for aggregating BirdNET output from batch runs or large-scale deployments.
#' It uses a fixed column specification to ensure consistent parsing of each file. Files with incompatible
#' formats or errors are skipped and their names are printed with a warning.
#'
#' @examples
#' \dontrun{
#' data <- birdnet_combine("path/to/BirdNET/output")
#' }
#'
#' @importFrom readr read_csv cols col_double col_character
#' @importFrom purrr map_dfr
#' @importFrom dplyr tibble
#' @importFrom cli cli_alert_success cli_alert_warning cli_li
#' @export

birdnet_combine <- function(path) {
  # argument check ----------------------------------------------------------
  checkmate::assert_character(path, len = 1, any.missing = FALSE)
  checkmate::assert_directory_exists(path)

  # list and filter files ---------------------------------------------------
  all_files <- list.files(
    path,
    pattern = "\\.(csv|txt)$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(all_files) == 0) {
    rlang::abort("No valid .csv or .txt files found in the specified directory.")
  }

  filtered_files <- all_files[!grepl("analysis_params|CombinedTable", all_files)]

  if (length(filtered_files) == 0) {
    rlang::abort("No valid .csv or .txt files found in the specified directory.")
  }

  # initialize containers ---------------------------------------------------
  results <- tibble::tibble()
  error_files <- c()

  # process files one by one ------------------------------------------------
  for (file in filtered_files) {

    detection_ind <- tryCatch({

      df <- readr::read_csv(file, show_col_types = FALSE)

      if (nrow(df) == 0) {
        next
      }

      # If not empty, append to results
      results <- dplyr::bind_rows(results, df)


    }, error = function(e) {
      error_files <<- c(error_files, file)
      NA
    })
  }

  # reporting ---------------------------------------------------------------
  if (length(error_files) > 0) {
    cli::cli_alert_success("Successfully combined {length(filtered_files) - length(error_files)} BirdNET output file{?s}")
    cli::cli_alert_warning("The following {length(error_files)} file{?s} caused errors and were skipped:")
    cli::cli_li(error_files)
  } else {
    cli::cli_alert_success("Successfully combined {length(filtered_files)} BirdNET output file{?s}")
  }

  return(results)
}
