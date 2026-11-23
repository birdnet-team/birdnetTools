# Tests for the validation Shiny app (folder + table + clips workflow)


# Helpers --------------------------------------------------------------------
# A validation folder like the one birdnet_extract_clips() makes:
# - row 1: a 9-second clip that covers 97-106 s of the original recording
# - row 2: a detection whose clip was never created (NA)
# - validation.csv: the table describing them
# The folder is deleted when the calling test finishes.
make_validation_folder <- function(with_clip_columns = TRUE,
                                   env = parent.frame()) {

  folder <- withr::local_tempdir(.local_envir = env)

  clip_name <- "01_site1_20230518_133500_100_103_American_Robin_0.950.wav"

  table <- data.frame(
    filepath = c("C:/recordings/site1_20230518_133500.wav",
                 "C:/recordings/site2_20230519_050000.wav"),
    scientific_name = c("Turdus migratorius", "Sturnus vulgaris"),
    common_name = c("American Robin", "European Starling"),
    start = c(100, 200),
    end = c(103, 203),
    confidence = c(0.95, 0.85)
  )

  if (with_clip_columns) {
    table$clip_file  <- c(clip_name, NA)
    table$clip_start <- c(97, NA)
    table$clip_end   <- c(106, NA)

    tuneR::writeWave(
      tuneR::sine(440, duration = 9, xunit = "time", samp.rate = 8000),
      file.path(folder, clip_name)
    )
  }

  utils::write.csv(table, file.path(folder, "validation.csv"), row.names = FALSE)

  folder
}

# Pretend the user picked `folder` with the folder button
mock_folder <- function(folder, env = parent.frame()) {
  testthat::local_mocked_bindings(
    parseDirPath = function(roots, selection, ...) folder,
    .package = "shinyFiles",
    .env = env
  )
}

# Capture what would be played, instead of playing it
mock_play <- function(env = parent.frame()) {
  played <- new.env()
  played$wave <- NULL

  testthat::local_mocked_bindings(
    play = function(object, ...) {
      played$wave <- object
      invisible(NULL)
    },
    .package = "tuneR",
    .env = env
  )

  played
}

# length of a Wave object in seconds
duration_of <- function(wave) {
  length(wave@left) / wave@samp.rate
}


# UI -------------------------------------------------------------------------

test_that("UI builds correctly", {
  ui <- birdnet_validation_ui(NULL)
  html <- paste(as.character(ui), collapse = "")

  expect_s3_class(ui, "shiny.tag.list")

  expect_true(grepl("table_file", html))
  expect_true(grepl("main_table", html))
  expect_true(grepl("spectrogram", html))
  expect_true(grepl("birdnetTools - Audio Validation Platform", html))

  # the file upload and the download button are gone
  expect_false(grepl("import_file", html))
  expect_false(grepl("save_file", html))
})


# Choosing a folder and a table ------------------------------------------------

test_that("choosing a folder finds the clips in it", {
  folder <- make_validation_folder()
  mock_folder(folder)

  shiny::testServer(birdnet_validation_server, {
    session$setInputs(dir = list(root = "Working_dir", path = list("")))

    expect_equal(normalizePath(dir_path()), normalizePath(folder))

    index <- audio_index()
    expect_equal(nrow(index), 1)
    expect_match(index$name, "American_Robin")
  })
})

test_that("the table is read from the chosen folder", {
  folder <- make_validation_folder()
  mock_folder(folder)

  shiny::testServer(birdnet_validation_server, {
    session$setInputs(table_file = "validation.csv")

    expect_equal(nrow(rv$data_editable), 2)
    expect_true(all(rv$data_editable$validation == "U"))
    expect_true(rv$has_clip)
    expect_equal(normalizePath(rv$csv_path),
                 normalizePath(file.path(folder, "validation.csv")))

    # display table: buttons added, recording shown by file name only
    expect_true(all(c("Spectrogram", "Audio") %in% names(rv$data_display)))
    expect_equal(rv$data_display[[rv$cols$filepath]][1],
                 "site1_20230518_133500.wav")
  })
})

test_that("a table without clip columns is still accepted", {
  folder <- make_validation_folder(with_clip_columns = FALSE)
  mock_folder(folder)

  shiny::testServer(birdnet_validation_server, {
    session$setInputs(table_file = "validation.csv")

    expect_false(rv$has_clip)
    expect_false(is.null(rv$data_display))
  })
})

test_that("nothing is loaded until a folder is chosen", {
  shiny::testServer(birdnet_validation_server, {
    session$setInputs(table_file = "validation.csv")

    expect_null(rv$data_editable)
    expect_null(rv$data_display)
  })
})

test_that("a table that is not in the folder is ignored", {
  folder <- make_validation_folder()
  mock_folder(folder)

  shiny::testServer(birdnet_validation_server, {
    session$setInputs(table_file = "not_here.csv")

    expect_null(rv$data_editable)
  })
})


# Editing and autosave -----------------------------------------------------------

test_that("editing a validation cell updates the table and saves it to the folder", {
  folder <- make_validation_folder()
  mock_folder(folder)

  shiny::testServer(birdnet_validation_server, {
    session$setInputs(table_file = "validation.csv")

    # column 6 is the validation column in the display table
    session$setInputs(main_table_cell_edit = list(row = 1, col = 6, value = "Y"))

    expect_equal(rv$data_editable$validation[1], "Y")

    # the change is already on disk, and the other columns are kept
    saved <- readr::read_csv(file.path(folder, "validation.csv"),
                             show_col_types = FALSE)
    expect_equal(saved$validation, c("Y", "U"))
    expect_true(all(c("clip_file", "clip_start", "clip_end") %in% names(saved)))
  })
})

test_that("validation can be resumed in a new session", {
  folder <- make_validation_folder()
  mock_folder(folder)

  shiny::testServer(birdnet_validation_server, {
    session$setInputs(table_file = "validation.csv")
    session$setInputs(main_table_cell_edit = list(row = 1, col = 6, value = "Y"))
  })

  shiny::testServer(birdnet_validation_server, {
    session$setInputs(table_file = "validation.csv")

    expect_equal(rv$data_editable$validation, c("Y", "U"))
  })
})

test_that("a failed save does not end the session", {
  folder <- make_validation_folder()
  mock_folder(folder)

  shiny::testServer(birdnet_validation_server, {
    session$setInputs(table_file = "validation.csv")

    # the folder disappears, for example because a drive was unplugged
    unlink(folder, recursive = TRUE)

    expect_no_error(
      session$setInputs(main_table_cell_edit = list(row = 1, col = 6, value = "Y"))
    )
    expect_equal(rv$data_editable$validation[1], "Y")
  })
})


# Buttons ----------------------------------------------------------------------

test_that("Praise button triggers notification", {
  shiny::testServer(birdnet_validation_server, {
    expect_silent({
      session$setInputs(praise_me = 1)
    })
  })
})


# Clicking Spectrogram / Audio ---------------------------------------------------

test_that("clicking with a NULL value does nothing", {
  shiny::testServer(birdnet_validation_server, {
    expect_no_error(
      session$setInputs(main_table_cell_clicked = list(value = NULL))
    )
  })
})

test_that("clicks before a folder is chosen are ignored", {
  shiny::testServer(birdnet_validation_server, {
    expect_no_error(
      session$setInputs(
        main_table_cell_clicked = list(row = 1, col = 7, value = "Spectrogram")
      )
    )
  })
})

test_that("a detection without a clip is handled gracefully", {
  folder <- make_validation_folder()
  mock_folder(folder)
  played <- mock_play()

  shiny::testServer(birdnet_validation_server, {
    session$setInputs(table_file = "validation.csv", duration = 9)

    # row 2 has NA in clip_file
    expect_no_error(
      session$setInputs(
        main_table_cell_clicked = list(row = 2, col = 8, value = "Audio")
      )
    )
    expect_null(played$wave)
  })
})

test_that("a clip that has gone missing from the folder is handled gracefully", {
  folder <- make_validation_folder()
  mock_folder(folder)
  played <- mock_play()

  unlink(list.files(folder, pattern = "\\.wav$", full.names = TRUE))

  shiny::testServer(birdnet_validation_server, {
    session$setInputs(table_file = "validation.csv", duration = 9)

    expect_no_error(
      session$setInputs(
        main_table_cell_clicked = list(row = 1, col = 8, value = "Audio")
      )
    )
    expect_null(played$wave)
  })
})

test_that("a clip moved into a subfolder is still found", {
  folder <- make_validation_folder()
  mock_folder(folder)
  played <- mock_play()

  clip <- list.files(folder, pattern = "\\.wav$")
  dir.create(file.path(folder, "sorted"))
  file.rename(file.path(folder, clip), file.path(folder, "sorted", clip))

  shiny::testServer(birdnet_validation_server, {
    session$setInputs(table_file = "validation.csv", duration = 9)
    session$setInputs(
      main_table_cell_clicked = list(row = 1, col = 8, value = "Audio")
    )

    expect_false(is.null(played$wave))
  })
})

# The detection is at 100-103 s in the recording, and the clip starts at 97 s,
# so the detection sits at 3-6 s inside the 9-second clip. The window shown is
# the detection plus (duration - 3) / 2 on each side, and can never run past
# the ends of the clip.
cases <- list(
  list(duration = 3,  expected = 3),   # detection only: 3-6 s
  list(duration = 9,  expected = 9),   # 0-9 s: the whole clip
  list(duration = 21, expected = 9)    # would be 0-15 s, trimmed to the clip
)

for (case in cases) {
  test_that(
    paste0("playback with duration = ", case$duration, " stays inside the clip"),
    {
      folder <- make_validation_folder()
      mock_folder(folder)
      played <- mock_play()
      expected <- case$expected
      duration <- case$duration

      shiny::testServer(birdnet_validation_server, {
        session$setInputs(table_file = "validation.csv", duration = duration)
        session$setInputs(
          main_table_cell_clicked = list(row = 1, col = 8, value = "Audio")
        )

        expect_false(is.null(played$wave))
        expect_equal(duration_of(played$wave), expected, tolerance = 0.05)
      })
    }
  )
}

test_that("clicking Spectrogram on a valid clip does not error", {
  folder <- make_validation_folder()
  mock_folder(folder)

  shiny::testServer(birdnet_validation_server, {
    session$setInputs(table_file = "validation.csv", duration = 9,
                      flim = c(0, 3), wl = 512)

    # the plot itself is only drawn when something asks for it
    expect_no_error(
      session$setInputs(
        main_table_cell_clicked = list(row = 1, col = 7, value = "Spectrogram")
      )
    )
  })
})


# Launching ----------------------------------------------------------------------

test_that("birdnet_launch_validation returns a Shiny app", {
  app <- birdnet_launch_validation()

  expect_s3_class(app, "shiny.appobj")
})


# Finding the audio (no Shiny needed) ---------------------------------------------

test_that("shared_suffix_length counts matching trailing folders", {
  expect_equal(
    shared_suffix_length("/a/site1/rec/x.wav", "/b/site1/rec/x.wav"), 3
  )

  # Windows-style and Unix-style paths can be compared
  expect_equal(
    shared_suffix_length("C:\\a\\site1\\x.wav", "/b/site1/x.wav"), 2
  )

  expect_equal(shared_suffix_length("/a/x.wav", "/b/y.wav"), 0)
})

test_that("resolve_audio_path uses the original path when the file exists", {
  f <- withr::local_tempfile(fileext = ".wav")
  file.create(f)

  expect_equal(resolve_audio_path(f, NULL), f)
})

test_that("resolve_audio_path finds a moved file by name", {
  index <- data.frame(
    path = c("/new/place/a.wav", "/new/place/b.wav"),
    name = c("a.wav", "b.wav")
  )

  # also when the original path uses Windows-style slashes
  expect_equal(resolve_audio_path("E:\\old\\b.wav", index), "/new/place/b.wav")
})

test_that("resolve_audio_path prefers the best matching folders when names collide", {
  index <- data.frame(
    path = c("/x/site1/a.wav", "/x/site2/a.wav"),
    name = c("a.wav", "a.wav")
  )

  expect_equal(resolve_audio_path("E:/old/site2/a.wav", index), "/x/site2/a.wav")
})

test_that("resolve_audio_path returns NA when nothing matches", {
  index <- data.frame(path = "/new/a.wav", name = "a.wav")

  expect_true(is.na(resolve_audio_path("E:/old/zzz.wav", index)))
  expect_true(is.na(resolve_audio_path(NA_character_, index)))
  expect_true(is.na(resolve_audio_path("E:/old/a.wav", NULL)))
})

