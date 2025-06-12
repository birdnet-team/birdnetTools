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



