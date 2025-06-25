#' Combine BirdNET output files from a directory
#'
#' Reads and combines multiple BirdNET output `.csv` or `.txt` files from a specified directory
#' into a single data frame. Subdirectories are searched recursively. Files named with
#' `"analysis_params"` or `"CombinedTable"` are automatically excluded.
#'
#' @param path Character string. Path to the directory containing BirdNET output files.
#'
#' @return A data frame combining all valid BirdNET output files found in the directory.
#' The column names and structure depend on the input files and are not standardized by this function.
#'
#' @details
#' Files are read using `readr::read_csv()`. Files that are empty or cannot be read are skipped.
#' A summary of the number of files combined and any files skipped due to error is printed at the end.
#'
#' This function is useful for aggregating large batches of BirdNET results without requiring consistent formatting.
#'
#' @examples
#' \dontrun{
#' data <- birdnet_combine("path/to/BirdNET/output")
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr bind_rows tibble
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
  results <- dplyr::tibble()
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
