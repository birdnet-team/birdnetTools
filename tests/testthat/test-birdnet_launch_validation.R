test_that("Shiny server accepts file from the input filepath import_file", {

  testServer(birdnet_validation_server, {
    session$setInputs(import_file = list(
      datapath = testthat::test_path("data", "test_example_1.csv"),
      name = "test_example_1.csv",
      size = file.info(testthat::test_path("data", "test_example_1.csv"))$size,
      type = "text/csv"
    ))

    # Trigger the observer and allow it time to run
    session$flushReact()

    # tests for the rv$data_editable
    expect_true(!is.null(rv$data_editable))

  })

})

