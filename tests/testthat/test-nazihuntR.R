context("basic functionality")
test_that("we can do something", {
  expect_that(import_nazis(table_name = 'all'), is_a("data.frame"))

})
