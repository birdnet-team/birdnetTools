
#' Calculate recording effort by site and date
#'
#' Scans a directory for all audio files, extracts site and datetime
#' metadata from their paths/filenames, and returns a unique timeline
#' of recording effort.
#'
#' The function identifies audio files matching common extensions, automatically
#' detects site names via [birdnet_add_site()], parses dates via
#' [birdnet_add_datetime()], and reduces the output to a unique combination
#' of sites and dates.
#'
#' @param path A character string specifying the path to the directory
#'   containing the audio files.
#'
#' @return A tibble (data frame) with two columns:
#' \describe{
#'   \item{site}{The extracted site identifier.}
#'   \item{date}{The date on which recording effort occurred.}
#' }
#'
#' @examples
#' \dontrun{
#' effort_df <- birdnet_get_effort("path/to/audio/storage")
#' head(effort_df)
#' }
#'
#' @export
birdnet_get_effort <- function(path) {

  # argument check ----------------------------------------------------------

  # ensure the directory actually exists before processing
  if (!dir.exists(path)) {
    stop(paste0("The directory '", path, "' does not exist."))
  }


  # main function -----------------------------------------------------------

  # find files and build the effort dataframe
  effort <- path |>
    # list the file names
    list.files(recursive = TRUE,
               full.names = TRUE,
               pattern = "\\.(wav|mp3|m4a|flac|ogg|wma)$",
               ignore.case = TRUE) |>
    # convert to tibble for processing, extract time and location
    tibble::tibble(filepath = _) |>
    birdnet_add_datetime() |>
    birdnet_add_site() |>
    # keep only the relevant columns and unique rows
    dplyr::select(site, date) |>
    dplyr::distinct()

  return(effort)
}
