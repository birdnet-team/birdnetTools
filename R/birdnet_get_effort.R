#' Calculate recording effort by site and date
#'
#' Scans a directory for all audio files, extracts site and datetime
#' metadata from their paths/filenames, and returns a unique timeline
#' of recording effort with file counts.
#'
#' The function identifies audio files matching common extensions, automatically
#' detects site names via [birdnet_add_site()], parses dates via
#' [birdnet_add_datetime()], and reduces the output to a unique combination
#' of sites and dates, summarizing total files recorded.
#'
#' @param path A character string specifying the path to the directory
#'   containing the audio files.
#' @param i An integer specifying the index of the path element to extract
#'   as the site identifier when split by slashes. Defaults to `-2`, which
#'   corresponds to the immediate parent directory of the file, passed directly
#'   to [birdnet_add_site()]. Negative values count from the right-hand side.
#'
#' @return A tibble (data frame) with three columns:
#' \describe{
#'   \item{site}{The extracted site identifier.}
#'   \item{date}{The date on which recording effort occurred.}
#'   \item{n_files}{Integer. The total number of audio files recorded at that
#'     site on that specific date.}
#' }
#'
#' @examples
#' \dontrun{
#' effort_df <- birdnet_get_effort("path/to/audio/storage", i = -2)
#' head(effort_df)
#' }
#'
#' @export
birdnet_get_effort <- function(path, i = -2) {


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
    (\(x) data.frame(filepath = x, stringsAsFactors = FALSE))() |>
    birdnet_add_datetime() |>
    birdnet_add_site(i = i) |>
    #dplyr::mutate(duration_mins = sapply(filepath, get_audio_duration))

    # keep only the relevant columns and unique rows
    dplyr::group_by("site", "date") |>
    dplyr::summarise(n_files = dplyr::n())


  return(effort)
}

