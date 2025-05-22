






group_BirdNET_output <- function(path){


  # Function ----------------------------------------------------------------


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
    purrr::map_dfr(~ {tryCatch(readr::read_csv(.x, col_types = column_spec),

                               error = function(e) {

                                 # Store the filename that caused the error
                                 error_files <<- c(error_files, .x)

                                 # Return an empty tibble with the same column structure to continue
                                 tibble(`Start (s)` = double(), `End (s)` = double(),
                                        `Scientific name` = character(), `Common name` = character(),
                                        `Confidence` = double(), `File` = character())
                               })})

  # Print the names of any files that caused errors
  if (length(error_files) > 0) {
    message("The following files caused errors and were skipped:")
    print(error_files)
  } else {
    message("All files loaded successfully.")
  }

  return(detections_raw)
}

