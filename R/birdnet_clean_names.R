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
#' @export
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
