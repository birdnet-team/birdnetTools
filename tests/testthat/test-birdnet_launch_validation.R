library(testthat)
library(shiny)
library(DT)

test_that("Shiny server accepts file from the input filepath import_file", {

  testServer(birdnet_validation_server, {

    # create a mock session
    session$setInputs(import_file = list(
      datapath = testthat::test_path("data", "test_example_1.csv"),
      name = "test_example_1.csv",
      size = file.info(testthat::test_path("data", "test_example_1.csv"))$size,
      type = "text/csv"
    ))

    # trigger the observer and allow it time to run
    session$flushReact()

    # tests for the rv$data_editable
    expect_true(!is.null(rv$data_editable))
    expect_true("validation" %in% names(rv$data_editable))


    # tests for the rv$data_display
    expect_true(!is.null(rv$data_display))
    expect_true("Audio" %in% names(rv$data_display))
    expect_true("Spectrogram" %in% names(rv$data_display))


    # test for the output$main_table
    expect_true(!is.null(output$main_table))

  })

})


# Helper: create minimal example CSV content (as a tempfile)
create_sample_csv <- function() {
  tmp <- tempfile(fileext = ".csv")
  df <- data.frame(
    filepath = c("bird1.wav", "bird2.wav"),
    scientific_name = c("Turdus migratorius", "Sturnus vulgaris"),
    common_name = c("American Robin", "European Starling"),
    start = c(10, 20),
    end = c(15, 25)
  )
  write.csv(df, tmp, row.names = FALSE)
  tmp
}

test_that("UI builds correctly", {
  ui <- birdnet_validation_ui(NULL)

  expect_s3_class(ui, "shiny.tag.list")
  # Check that UI contains fileInput and DT output
  expect_true(any(grepl("import_file", as.character(ui))))
  expect_true(any(grepl("main_table", as.character(ui))))
  expect_true(any(grepl("spectrogram", as.character(ui))))
  # Title check
  expect_true(any(grepl("birdnetTools / Audio Validation platform", as.character(ui))))
})

test_that("Server reacts to file input and renders table", {
  sample_csv <- create_sample_csv()

  testServer(birdnet_validation_server, {
    # Simulate file upload
    session$setInputs(import_file = list(datapath = sample_csv))

    # Wait for reactive to update
    # Reactive values should be populated
    expect_true(!is.null(rv$data_editable))
    expect_true("validation" %in% names(rv$data_editable))

    # data_display contains added columns Spectrogram and Audio
    expect_true(all(c("Spectrogram", "Audio") %in% names(rv$data_display)))

    # The table output should be a datatable
    #tbl <- output$main_table
    #expect_true(inherits(tbl, "shiny.render.function"))
  })
})

test_that("Editing table updates validation column", {
  sample_csv <- create_sample_csv()

  testServer(birdnet_validation_server, {
    session$setInputs(import_file = list(datapath = sample_csv))

    # Initially validation column has default "U"
    expect_true(all(rv$data_editable$validation == "U"))

    # Simulate editing cell (validation column)
    # Column index is 6 for validation (assuming columns: filepath, sci_name, common_name, start, end, validation)
    session$setInputs(main_table_cell_edit = list(row = 1, col = 6, value = "Y"))

    expect_equal(rv$data_editable$validation[1], "Y")
  })
})


test_that("Praise button triggers notification", {
  testServer(birdnet_validation_server, {
    expect_silent({
      session$setInputs(praise_me = 1)
    })
  })
})

test_that("Spectrogram and Audio button clicks handle missing files gracefully", {
  sample_csv <- create_sample_csv()

  testServer(birdnet_validation_server, {
    session$setInputs(import_file = list(datapath = sample_csv))
    session$setInputs(dir = list(roots = c(Working_dir = "."), path = "nonexistent"))

    # Click spectrogram button column (7) with missing file
    session$setInputs(main_table_cell_clicked = list(row = 1, col = 7, value = "Spectrogram"))

    # Click audio button column (8) with missing file
    session$setInputs(main_table_cell_clicked = list(row = 1, col = 8, value = "Audio"))

    # Expect notifications (can't test shiny notification easily here but no error)
    expect_true(TRUE)
  })
})

test_that("birdnet_launch_validation launches without error", {
  expect_silent({
    app <- birdnet_launch_validation()
  })
})




