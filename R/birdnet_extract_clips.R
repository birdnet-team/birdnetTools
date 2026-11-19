#' Extract audio clips for BirdNET detections
#'
#' Cuts a short audio clip for each detection in a BirdNET output table and
#' saves all clips in one folder, for example to validate detections without
#' needing the original recordings. Each clip runs from the start of the
#' detection to its end, plus a buffer before and after.
#'
#' The detection table, with the clip columns added (see Value), is saved as a
#' CSV file in the same folder, so the folder holds everything needed to
#' validate the detections.
#'
#' **File names.** Clips are saved in the same audio format as the original
#' recording (e.g. WAV stays WAV, MP3 stays MP3) and are named
#' `<ID>_<recording name>_<start>_<end>_<species common name>_<confidence>.<extension>`,
#' where:
#' * `ID` is the row number of the detection in `data`, with leading zeros
#'   (`01`, `02`, ...), so the clips sort in the same order as the table.
#' * `start` and `end` are the detection times in seconds (without the buffer).
#' * the species common name has characters that are unsafe in file names
#'   replaced with `_`.
#' * `confidence` is the detection confidence, rounded to three decimals.
#'
#' For example, `01_20230518_133500_12_15_Barred_Owl_0.873.wav`. Because
#' every name starts with its own ID, names are always unique.
#'
#' **Finding the audio.** For each detection, the function first uses the file
#' path stored in the table. If that file no longer exists (for example because
#' the recordings were moved to another drive), it looks for a file with the
#' same name inside `audio_dir`, including all subfolders. If several files have
#' that name, the one whose parent folders best match the original path is
#' used.
#'
#' **Clips near the edges.** A clip is trimmed to fit inside the recording, so
#' a detection near the start or end of a file gets a shorter buffer on that
#' side. The times actually used are returned in `clip_start` and `clip_end`.
#'
#' @param data A data frame containing BirdNET output, with columns for the
#'   file path, start time, end time, species common name, and confidence
#'   (detected automatically).
#' @param output_dir Character. Folder where the clips are saved. It is created
#'   if it does not exist.
#' @param audio_dir Character or `NULL`. Folder containing the original audio
#'   recordings, searched including subfolders. Only used for files that are
#'   not found at the path stored in `data`. Defaults to `NULL`, which uses the
#'   stored paths only.
#' @param buffer Numeric. Seconds added before the start and after the end of
#'   each detection. Defaults to `3`.
#' @param file Character or `NULL`. Where to save the table as a CSV file (must
#'   end in `.csv`). Defaults to `NULL`, which saves it as `detections_clips.csv`
#'   in `output_dir`, next to the clips.
#' @param overwrite Logical. If `FALSE`, the function stops before extracting
#'   anything when the table file already exists, so that a table you have
#'   already validated is not replaced by accident. Defaults to `FALSE`.
#' @param quiet Logical. If `TRUE`, suppresses the status messages about how
#'   many clips were extracted or could not be created. Defaults to `FALSE`.
#'
#' @return The input data frame with additional columns:
#' \describe{
#'   \item{clip_file}{The name of the clip file in `output_dir`, or `NA` if the
#'   clip could not be created.}
#'   \item{clip_start}{Start of the clip in the original recording, in seconds.}
#'   \item{clip_end}{End of the clip in the original recording, in seconds.}
#' }
#'
#' @examples
#' \dontrun{
#' sampled <- birdnet_subsample(my_data, n = 100, seed = 1)
#'
#' clips <- birdnet_extract_clips(
#'   sampled,
#'   output_dir = "validation_audio",
#'   audio_dir = "path/to/recordings",
#'   buffer = 3
#' )
#' # "validation_audio" now holds the clips and detections_clips.csv
#' }
#'
#' @seealso [birdnet_subsample()], [birdnet_launch_validation()]
#' @export
birdnet_extract_clips <- function(data,
                                  output_dir,
                                  audio_dir = NULL,
                                  buffer = 3,
                                  file = NULL,
                                  overwrite = FALSE,
                                  quiet = FALSE) {

  # argument check ----------------------------------------------------------

  checkmate::assert_data_frame(data)
  checkmate::assert_string(output_dir)
  checkmate::assert_string(audio_dir, null.ok = TRUE)
  if (!is.null(audio_dir)) checkmate::assert_directory_exists(audio_dir)
  checkmate::assert_number(buffer, lower = 0)
  checkmate::assert_flag(overwrite)
  checkmate::assert_flag(quiet)

  if (!is.null(file)) {
    checkmate::assert_string(file)
    if (!grepl("\\.csv$", file, ignore.case = TRUE)) {
      cli::cli_abort("{.arg file} must end with {.val .csv}.")
    }
  }

  # detect columns
  cols <- birdnet_detect_columns(data)
  needed <- c("filepath", "start", "end", "common_name", "confidence")
  absent <- needed[vapply(needed, function(x) {
    is.null(cols[[x]]) || is.na(cols[[x]])
  }, logical(1))]

  if (length(absent) > 0) {
    cli::cli_abort("Can't find column{?s} for {.field {absent}} in {.arg data}.")
  }


  # helpers used only in this function --------------------------------------

  # audio formats to look for in audio_dir
  audio_extensions <- "\\.(wav|mp3|flac|ogg|m4a|aac|wma)$"

  # file name from a path, whichever slash style the path uses
  # (the table may have been made on Windows and be used on macOS or Linux)
  path_name <- function(x) {
    stringr::str_extract(x, "[^\\\\/]+$")
  }

  # format a time in seconds for a file name: no scientific notation, no padding
  format_time <- function(x) {
    vapply(x, function(v) format(v, trim = TRUE, scientific = FALSE), character(1))
  }


  # main function -----------------------------------------------------------

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # list the audio in audio_dir once, rather than once per detection
  index <- NULL
  if (!is.null(audio_dir)) {
    files <- list.files(audio_dir, pattern = audio_extensions, recursive = TRUE,
                        full.names = TRUE, ignore.case = TRUE)
    index <- data.frame(path = files, name = basename(files),
                        stringsAsFactors = FALSE)
  }

  originals <- as.character(data[[cols$filepath]])
  starts    <- data[[cols$start]]
  ends      <- data[[cols$end]]

  # where is each recording now?
  source_paths <- vapply(originals, resolve_audio_path, character(1),
                         index = index, USE.NAMES = FALSE)

  # clip names: ID _ recording name _ start _ end _ species _ confidence . extension
  n_total <- nrow(data)

  # IDs follow the row order of the table, padded to the same width (01, 02, ...)
  ids <- formatC(seq_len(n_total), width = max(2, nchar(n_total)), flag = "0")

  species_clean <- data[[cols$common_name]] |>
    as.character() |>
    stringr::str_replace_all("[^\\p{L}\\p{N}]+", "_") |>
    stringr::str_remove_all("^_+|_+$")

  clip_base <- paste(
    ids,
    tools::file_path_sans_ext(path_name(originals)),
    format_time(starts),
    format_time(ends),
    species_clean,
    sprintf("%.3f", as.numeric(data[[cols$confidence]])),
    sep = "_"
  )
  clip_name <- paste0(clip_base, ".", tools::file_ext(path_name(originals)))

  # cut the clips
  out_file  <- rep(NA_character_, n_total)
  out_start <- rep(NA_real_, n_total)
  out_end   <- rep(NA_real_, n_total)
  status    <- rep("ok", n_total)

  if (!quiet && n_total > 0) {
    cli::cli_progress_bar("Extracting clips", total = n_total)
  }

  for (r in seq_len(n_total)) {

    if (is.na(source_paths[r])) {
      status[r] <- "not_found"

    } else {
      used <- tryCatch(
        clip_audio(
          input  = source_paths[r],
          output = file.path(output_dir, clip_name[r]),
          from   = starts[r] - buffer,
          to     = ends[r] + buffer
        ),
        error = function(e) NULL
      )

      if (is.null(used)) {
        status[r] <- "error"
      } else {
        out_file[r]  <- clip_name[r]
        out_start[r] <- used[1]
        out_end[r]   <- used[2]
      }
    }

    if (!quiet && n_total > 0) cli::cli_progress_update()
  }

  data$clip_file  <- out_file
  data$clip_start <- out_start
  data$clip_end   <- out_end


  # tell the user what happened ---------------------------------------------

  n_ok      <- sum(status == "ok")
  n_missing <- sum(status == "not_found")
  n_error   <- sum(status == "error")

  if (!quiet) {
    if (n_ok > 0) {
      cli::cli_alert_success(
        "Extracted {n_ok} of {n_total} clip{?s} to {.path {output_dir}}."
      )
    }

    if (n_missing > 0) {
      cli::cli_alert_warning(
        "{n_missing} audio file{?s} could not be found, so no clip was made."
      )
      cli::cli_alert_info(
        "Check {.arg audio_dir} and the file paths in your table."
      )
    }

    if (n_error > 0) {
      cli::cli_alert_warning(
        "{n_error} clip{?s} could not be written. Their {.field clip_file} is {.val NA}."
      )
    }
  }

  # optional write to CSV
  if (!is.null(file)) {
    readr::write_csv(data, file, progress = FALSE)
  }

  return(data)
}


# Helpers --------------------------------------------------------------------
# clip_audio() is only used by birdnet_extract_clips(), so it can stay in this
# file. shared_suffix_length() and resolve_audio_path() are general-purpose
# (the validation app may use them too), so they could move to R/utils.R.

# Cut one clip out of an audio file and write it in the same format.
# WAV files are read and written with tuneR, which keeps bit depth and reads only
# the part that is needed. Other formats (mp3, flac, ...) go through av.
# Returns the start and end (in seconds) that were actually used, so a clip that
# runs past the start or end of the recording is trimmed to fit.
clip_audio <- function(input, output, from, to) {

  from <- max(0, from)

  if (tolower(tools::file_ext(input)) == "wav") {
    header <- tuneR::readWave(input, header = TRUE)
    to <- min(header$samples / header$sample.rate, to)
    if (to <= from) stop("The clip is empty.")

    clip <- tuneR::readWave(input, from = from, to = to, units = "seconds")
    tuneR::writeWave(clip, output)

  } else {
    to <- min(av::av_media_info(input)$duration, to)
    if (to <= from) stop("The clip is empty.")

    # start_time and total_time cut the audio; the output format follows the
    # file extension
    av::av_audio_convert(input, output,
                         start_time = from,
                         total_time = to - from,
                         verbose = FALSE)
  }

  c(from, to)
}



