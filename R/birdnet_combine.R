#' Combine BirdNET output files from a directory
#'
#' Reads and combines multiple BirdNET output `.csv` or `.txt` files from a specified directory
#' (including subdirectories) into a single data frame.
#'
#' Files named `"analysis_params"` or `"CombinedTable"` are automatically excluded. Files that are
#' empty or cannot be read are skipped. A summary of how many files were successfully read and which
#' files (if any) caused errors is printed at the end.
#'
#' @param path Character string. Path to a directory containing BirdNET output files.
#'
#' @return A data frame combining all readable BirdNET `.csv` or `.txt` files found in the directory.
#'   The column structure depends on the input files and is not standardized by this function.
#'
#' @details
#' This function is useful for aggregating large batches of BirdNET results without requiring
#' consistent formatting. All subdirectories are searched recursively. Files are read using
#' [readr::read_delim()], and empty files are silently ignored.
#'
#' If no valid files are found, or if all files are excluded or cause read errors, the function
#' aborts with an informative message.
#'
#' @examples
#' \dontrun{
#' # Combine all BirdNET output files in a directory and its subfolders
#' data <- birdnet_combine("path/to/BirdNET/output")
#'
#' # View a few rows of the result
#' head(data)
#' }
#'
#' @seealso [readr::read_delim()], [list.files()]
#'
#' @importFrom readr read_delim
#' @importFrom dplyr bind_rows tibble
#' @importFrom cli cli_alert_success cli_alert_warning cli_li
#' @export

birdnet_combine <- function(path,
                            add_filepath = FALSE) {

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

      df <- readr::read_delim(file, show_col_types = FALSE)

      # mutate a column called filepath if the add_filepath argument is TRUE
      if (add_filepath) {
        df <- df %>%
          dplyr::mutate(filepath = file)
      }


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
