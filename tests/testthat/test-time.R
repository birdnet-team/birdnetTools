test_that("what_time() works", {
  expect_type(what_time(), "character")
  expect_snapshot(what_time(city = "bla"), error = TRUE)
  expect_no_error(what_time(city = "Vancouver"))
  expect_no_error(what_time(city = "Chemnitz"))
})
