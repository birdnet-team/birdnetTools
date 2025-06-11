test_that("shinyapp can launch after calling", {
  expect_equal()
})


test_that("Initial test for the server", {

  testServer(birdnet_validation_server, {
    session$setInputs(import_file = list(
      datapath = testthat::test_path("data", "test_example_1.csv"),
      name = "test_example_1.csv",
      size = file.info(testthat::test_path("data", "test_example_1.csv"))$size,
      type = "text/csv"
    ))

    # Trigger the observer and allow it time to run
    session$flushReact()

    # Now you can test output or reactive values
    expect_true(!is.null(rv$data_display))
  })

})

