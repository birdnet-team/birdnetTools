

# Helpers --------------------------------------------------------------------

# A short test recording
make_recording <- function(path, seconds = 10) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tuneR::writeWave(
    tuneR::sine(440, duration = seconds, xunit = "time", samp.rate = 8000),
    path
  )
  invisible(path)
}

# A detection table with the columns birdnet_extract_clips() needs.
# Vectors give one row per element; single values are repeated.
make_detections <- function(filepath,
                            start = 3,
                            end = 6,
                            species = "American Robin",
                            confidence = 0.95) {
  data.frame(
    filepath = filepath,
    scientific_name = "Turdus migratorius",
    common_name = species,
    start = start,
    end = end,
    confidence = confidence,
    stringsAsFactors = FALSE
  )
}

# Length of a WAV file in seconds
wave_seconds <- function(path) {
  header <- tuneR::readWave(path, header = TRUE)
  header$samples / header$sample.rate
}

# Names of the clip files in a folder
clip_files <- function(folder, pattern = "\\.wav$") {
  list.files(folder, pattern = pattern, ignore.case = TRUE)
}

# Does this installation of av have an MP3 encoder?
has_mp3_encoder <- function() {
  encoders <- tryCatch(av::av_encoders(), error = function(e) NULL)
  !is.null(encoders) && "libmp3lame" %in% encoders$name
}


# Arguments --------------------------------------------------------------------

test_that("invalid arguments are rejected", {
  out <- withr::local_tempdir()
  det <- make_detections("a.wav")

  expect_error(birdnet_extract_clips("not a data frame", output_dir = out))
  expect_error(birdnet_extract_clips(det, output_dir = 1))
  expect_error(birdnet_extract_clips(det, output_dir = out,
                                     audio_dir = file.path(out, "nope")))
  expect_error(birdnet_extract_clips(det, output_dir = out, buffer = -1),
               "buffer")
  expect_error(birdnet_extract_clips(det, output_dir = out, buffer = "3"))
  expect_error(birdnet_extract_clips(det, output_dir = out, overwrite = "yes"))
  expect_error(birdnet_extract_clips(det, output_dir = out, quiet = NA))
})

test_that("file must be a single path ending in .csv", {
  out <- withr::local_tempdir()
  det <- make_detections("a.wav")

  expect_error(
    birdnet_extract_clips(det, output_dir = out, file = "table.txt"),
    "must end with"
  )
  expect_error(
    birdnet_extract_clips(det, output_dir = out, file = c("a.csv", "b.csv"))
  )
})

test_that("a missing column stops the function before anything is created", {
  new_dir <- file.path(withr::local_tempdir(), "not_created")
  det <- make_detections("a.wav")
  det$confidence <- NULL

  expect_error(birdnet_extract_clips(det, output_dir = new_dir))
  expect_false(dir.exists(new_dir))
})


# Cutting and naming clips -------------------------------------------------------

test_that("a clip is cut around the detection and named correctly", {
  recordings <- withr::local_tempdir()
  out <- withr::local_tempdir()
  rec <- file.path(recordings, "site1_20230518_133500.wav")
  make_recording(rec, seconds = 10)

  result <- birdnet_extract_clips(
    make_detections(rec, start = 3, end = 6, confidence = 0.95),
    output_dir = out,
    quiet = TRUE
  )

  expected <- "01_site1_20230518_133500_3_6_American_Robin_0.950.wav"
  clip <- file.path(out, expected)

  expect_equal(result$clip_file, expected)
  expect_true(file.exists(clip))

  # detection 3-6 s plus a 3 s buffer on each side is 0-9 s of the recording
  expect_equal(result$clip_start, 0)
  expect_equal(result$clip_end, 9)
  expect_equal(wave_seconds(clip), 9, tolerance = 0.01)

  # the audio format is kept
  header <- tuneR::readWave(clip, header = TRUE)
  expect_equal(header$sample.rate, 8000)
  expect_equal(header$bits, 32)
})

test_that("the returned table keeps the original columns and rows", {
  recordings <- withr::local_tempdir()
  out <- withr::local_tempdir()
  rec <- file.path(recordings, "rec.wav")
  make_recording(rec)

  det <- make_detections(rec, start = c(1, 3, 5), end = c(2, 4, 6))
  result <- birdnet_extract_clips(det, output_dir = out, quiet = TRUE)

  expect_equal(nrow(result), 3)
  expect_equal(result[names(det)], det)
  expect_true(all(c("clip_file", "clip_start", "clip_end") %in% names(result)))
})

test_that("buffer = 0 gives just the detection", {
  recordings <- withr::local_tempdir()
  out <- withr::local_tempdir()
  rec <- file.path(recordings, "rec.wav")
  make_recording(rec)

  result <- birdnet_extract_clips(
    make_detections(rec, start = 3, end = 6),
    output_dir = out,
    buffer = 0,
    quiet = TRUE
  )

  expect_equal(result$clip_start, 3)
  expect_equal(result$clip_end, 6)
  expect_equal(wave_seconds(file.path(out, result$clip_file)), 3, tolerance = 0.01)
})

test_that("clips near the edges of a recording are trimmed to fit", {
  recordings <- withr::local_tempdir()
  out <- withr::local_tempdir()
  rec <- file.path(recordings, "rec.wav")
  make_recording(rec, seconds = 10)

  # one detection near the start, one near the end
  result <- birdnet_extract_clips(
    make_detections(rec, start = c(1, 8), end = c(2, 9.5)),
    output_dir = out,
    quiet = TRUE
  )

  expect_equal(result$clip_start, c(0, 5))
  expect_equal(result$clip_end, c(5, 10), tolerance = 0.01)
  expect_equal(wave_seconds(file.path(out, result$clip_file[1])), 5, tolerance = 0.01)
  expect_equal(wave_seconds(file.path(out, result$clip_file[2])), 5, tolerance = 0.01)
})

test_that("times with decimals and the confidence are written into the name", {
  recordings <- withr::local_tempdir()
  out <- withr::local_tempdir()
  rec <- file.path(recordings, "rec.wav")
  make_recording(rec)

  result <- birdnet_extract_clips(
    make_detections(rec, start = 1.5, end = 4.5, confidence = 0.8734),
    output_dir = out,
    quiet = TRUE
  )

  expect_match(result$clip_file, "_1.5_4.5_", fixed = TRUE)
  expect_match(result$clip_file, "_0.873.wav", fixed = TRUE)
})

test_that("unsafe characters in the species name are replaced", {
  recordings <- withr::local_tempdir()
  out <- withr::local_tempdir()
  rec <- file.path(recordings, "rec.wav")
  make_recording(rec)

  result <- birdnet_extract_clips(
    make_detections(
      rec,
      start = c(3, 3, 3),
      end = c(6, 6, 6),
      species = c("Swainson's Thrush", "Grey-headed Woodpecker.", "Barred Owl")
    ),
    output_dir = out,
    quiet = TRUE
  )

  expect_match(result$clip_file[1], "_Swainson_s_Thrush_", fixed = TRUE)
  expect_match(result$clip_file[2], "_Grey_headed_Woodpecker_", fixed = TRUE)
  expect_match(result$clip_file[3], "_Barred_Owl_", fixed = TRUE)

  # no doubled underscores, for example from the trailing "."
  expect_false(any(grepl("__", result$clip_file, fixed = TRUE)))
})

test_that("IDs follow the table order and are padded to the same width", {
  recordings <- withr::local_tempdir()
  out <- withr::local_tempdir()
  rec <- file.path(recordings, "rec.wav")
  make_recording(rec)

  # 100 identical detections: the ID is what keeps the names apart
  result <- birdnet_extract_clips(
    make_detections(rec, start = rep(1, 100), end = rep(2, 100)),
    output_dir = out,
    buffer = 0.5,
    quiet = TRUE
  )

  expect_true(startsWith(result$clip_file[1], "001_"))
  expect_true(startsWith(result$clip_file[100], "100_"))
  expect_equal(anyDuplicated(result$clip_file), 0L)
  expect_equal(sort(result$clip_file), result$clip_file)
  expect_length(clip_files(out), 100)
})

test_that("the extension of the recording is kept as it is", {
  recordings <- withr::local_tempdir()
  out <- withr::local_tempdir()
  rec <- file.path(recordings, "SITE_A.WAV")
  make_recording(rec)

  result <- birdnet_extract_clips(make_detections(rec), output_dir = out,
                                  quiet = TRUE)

  expect_match(result$clip_file, "\\.WAV$")
  expect_true(file.exists(file.path(out, result$clip_file)))
})

test_that("MP3 recordings give MP3 clips", {
  skip_if_not(has_mp3_encoder(), "av has no MP3 encoder on this system")

  recordings <- withr::local_tempdir()
  out <- withr::local_tempdir()
  wav <- make_recording(file.path(recordings, "source.wav"))
  mp3 <- file.path(recordings, "rec.mp3")
  av::av_audio_convert(wav, mp3, verbose = FALSE)

  result <- birdnet_extract_clips(
    make_detections(mp3, start = 3, end = 6),
    output_dir = out,
    quiet = TRUE
  )

  clip <- file.path(out, result$clip_file)

  expect_match(result$clip_file, "\\.mp3$")
  expect_true(file.exists(clip))
  # MP3 encoding adds a little padding, so allow some slack
  expect_equal(av::av_media_info(clip)$duration, 9, tolerance = 0.05)
})

test_that("the output folder is created, including parent folders", {
  recordings <- withr::local_tempdir()
  out <- file.path(withr::local_tempdir(), "a", "b", "clips")
  rec <- file.path(recordings, "rec.wav")
  make_recording(rec)

  birdnet_extract_clips(make_detections(rec), output_dir = out, quiet = TRUE)

  expect_true(dir.exists(out))
  expect_length(clip_files(out), 1)
})

test_that("an empty table gives an empty result", {
  recordings <- withr::local_tempdir()
  out <- withr::local_tempdir()
  rec <- file.path(recordings, "rec.wav")
  make_recording(rec)

  result <- birdnet_extract_clips(make_detections(rec)[0, ], output_dir = out,
                                  quiet = TRUE)

  expect_equal(nrow(result), 0)
  expect_true(all(c("clip_file", "clip_start", "clip_end") %in% names(result)))
  expect_length(clip_files(out), 0)
})


# Finding the audio ------------------------------------------------------------

test_that("the stored path is used, and audio_dir is the fallback", {
  recordings <- withr::local_tempdir()
  out_without <- withr::local_tempdir()
  out_with <- withr::local_tempdir()

  # the recording lives in a subfolder; the table still points to an old drive
  make_recording(file.path(recordings, "siteA", "rec_20230518.wav"))
  det <- make_detections("E:\\old_drive\\siteA\\rec_20230518.wav")

  # without audio_dir the recording cannot be found
  not_found <- birdnet_extract_clips(det, output_dir = out_without, quiet = TRUE)
  expect_true(is.na(not_found$clip_file))
  expect_length(clip_files(out_without), 0)

  # with audio_dir it is found by name, subfolders included
  found <- birdnet_extract_clips(det, output_dir = out_with,
                                 audio_dir = recordings, quiet = TRUE)
  expect_false(is.na(found$clip_file))
  expect_match(found$clip_file, "^01_rec_20230518_3_6_")
  expect_true(file.exists(file.path(out_with, found$clip_file)))
})

test_that("the stored path is preferred when the file still exists", {
  originals <- withr::local_tempdir()
  other <- withr::local_tempdir()
  out <- withr::local_tempdir()

  # same file name in two places: a 10 s original and a 20 s look-alike
  original <- make_recording(file.path(originals, "rec.wav"), seconds = 10)
  make_recording(file.path(other, "rec.wav"), seconds = 20)

  result <- birdnet_extract_clips(
    make_detections(original, start = 8, end = 9),
    output_dir = out,
    audio_dir = other,
    buffer = 3,
    quiet = TRUE
  )

  # the 10 s original ends the clip at 10 s; the 20 s file would give 12 s
  expect_equal(result$clip_end, 10, tolerance = 0.01)
})

test_that("when names collide, the best matching parent folders win", {
  recordings <- withr::local_tempdir()
  out <- withr::local_tempdir()

  make_recording(file.path(recordings, "siteA", "rec.wav"), seconds = 10)
  make_recording(file.path(recordings, "siteB", "rec.wav"), seconds = 20)

  result <- birdnet_extract_clips(
    make_detections("E:/old/siteB/rec.wav", start = 12, end = 15),
    output_dir = out,
    audio_dir = recordings,
    buffer = 3,
    quiet = TRUE
  )

  # only the 20 s recording in siteB can give a clip ending at 18 s
  expect_equal(result$clip_end, 18)
})

test_that("recordings that cannot be found are reported and skipped", {
  recordings <- withr::local_tempdir()
  out <- withr::local_tempdir()
  rec <- file.path(recordings, "rec.wav")
  make_recording(rec)

  det <- make_detections(c(rec, "E:/gone/missing.wav", NA_character_))

  msgs <- testthat::capture_messages(
    result <- birdnet_extract_clips(det, output_dir = out)
  )

  expect_false(is.na(result$clip_file[1]))
  expect_true(all(is.na(result$clip_file[2:3])))
  expect_true(all(is.na(result$clip_start[2:3])))
  expect_true(all(is.na(result$clip_end[2:3])))
  expect_length(clip_files(out), 1)

  expect_match(paste(msgs, collapse = ""), "2 audio files could not be found")
})

test_that("a detection outside the recording gives NA and a message", {
  recordings <- withr::local_tempdir()
  out <- withr::local_tempdir()
  rec <- file.path(recordings, "rec.wav")
  make_recording(rec, seconds = 10)

  # 50-53 s in a 10 s recording
  msgs <- testthat::capture_messages(
    result <- birdnet_extract_clips(make_detections(rec, start = 50, end = 53),
                                    output_dir = out)
  )

  expect_true(is.na(result$clip_file))
  expect_length(clip_files(out), 0)
  expect_match(paste(msgs, collapse = ""), "could not be written")
})


# The saved table ------------------------------------------------------------------

test_that("the table is saved next to the clips by default", {
  recordings <- withr::local_tempdir()
  out <- withr::local_tempdir()
  rec <- file.path(recordings, "rec.wav")
  make_recording(rec)

  result <- birdnet_extract_clips(make_detections(rec), output_dir = out,
                                  quiet = TRUE)

  table_file <- file.path(out, "detections_clips.csv")
  expect_true(file.exists(table_file))

  saved <- readr::read_csv(table_file, show_col_types = FALSE)
  expect_equal(saved$clip_file, result$clip_file)
  expect_true(all(
    c("filepath", "common_name", "start", "end", "confidence",
      "clip_file", "clip_start", "clip_end") %in% names(saved)
  ))
})

test_that("a custom file is used instead of the default", {
  recordings <- withr::local_tempdir()
  out <- withr::local_tempdir()
  rec <- file.path(recordings, "rec.wav")
  make_recording(rec)

  # in a folder that does not exist yet, and with an upper-case extension
  custom <- file.path(withr::local_tempdir(), "tables", "mine.CSV")

  birdnet_extract_clips(make_detections(rec), output_dir = out, file = custom,
                        quiet = TRUE)

  expect_true(file.exists(custom))
  expect_false(file.exists(file.path(out, "detections_clips.csv")))
})

test_that("an existing table is never replaced by accident", {
  recordings <- withr::local_tempdir()
  out <- withr::local_tempdir()
  rec <- file.path(recordings, "rec.wav")
  make_recording(rec)
  det <- make_detections(rec)

  birdnet_extract_clips(det, output_dir = out, quiet = TRUE)

  # pretend the table has been validated in the app
  table_file <- file.path(out, "detections_clips.csv")
  saved <- readr::read_csv(table_file, show_col_types = FALSE)
  saved$validation <- "Y"
  readr::write_csv(saved, table_file)

  # a second run is refused, and it happens before any clip is cut
  unlink(file.path(out, clip_files(out)))
  expect_error(
    birdnet_extract_clips(det, output_dir = out, quiet = TRUE),
    "already exists"
  )
  expect_length(clip_files(out), 0)
  expect_equal(
    readr::read_csv(table_file, show_col_types = FALSE)$validation,
    "Y"
  )

  # overwrite = TRUE replaces it
  expect_no_error(
    birdnet_extract_clips(det, output_dir = out, overwrite = TRUE, quiet = TRUE)
  )
  expect_length(clip_files(out), 1)
  expect_false(
    "validation" %in% names(readr::read_csv(table_file, show_col_types = FALSE))
  )
})


# Messages ---------------------------------------------------------------------------

test_that("quiet = TRUE prints nothing", {
  recordings <- withr::local_tempdir()
  out <- withr::local_tempdir()
  rec <- file.path(recordings, "rec.wav")
  make_recording(rec)

  expect_silent(
    birdnet_extract_clips(make_detections(rec), output_dir = out, quiet = TRUE)
  )
})

test_that("the status messages report what happened", {
  recordings <- withr::local_tempdir()
  out <- withr::local_tempdir()
  rec <- file.path(recordings, "rec.wav")
  make_recording(rec)

  msgs <- testthat::capture_messages(
    birdnet_extract_clips(make_detections(rec, start = c(1, 3), end = c(2, 4)),
                          output_dir = out)
  )
  text <- paste(msgs, collapse = "")

  expect_match(text, "Extracted 2 of 2 clips")
  expect_match(text, "Saved the table")
})


# clip_audio() -------------------------------------------------------------------------

test_that("clip_audio trims to the recording and returns the times it used", {
  rec <- withr::local_tempfile(fileext = ".wav")
  out <- withr::local_tempfile(fileext = ".wav")
  make_recording(rec, seconds = 10)

  used <- clip_audio(rec, out, from = -2, to = 15)

  expect_equal(used[1], 0)
  expect_equal(used[2], 10, tolerance = 0.01)
  expect_equal(wave_seconds(out), 10, tolerance = 0.01)
})

test_that("clip_audio refuses an empty clip", {
  rec <- withr::local_tempfile(fileext = ".wav")
  out <- withr::local_tempfile(fileext = ".wav")
  make_recording(rec, seconds = 10)

  expect_error(clip_audio(rec, out, from = 12, to = 15), "empty")
  expect_false(file.exists(out))
})

