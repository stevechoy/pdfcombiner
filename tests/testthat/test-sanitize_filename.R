library(testthat)

interactive <- NULL

test_that("sanitize_filename replaces illegal characters", {
  # Mock shiny::showNotification
  local_mocked_bindings(
    showNotification = function(...) {}, # Mock function does nothing
    .package = "shiny"
  )

  # Test cases with illegal characters
  expect_equal(sanitize_filename("file:name.pdf"), "file_name.pdf")
  expect_equal(sanitize_filename("file*name.pdf"), "file_name.pdf")
  expect_equal(sanitize_filename("file?name.pdf"), "file_name.pdf")
  expect_equal(sanitize_filename("file|name.pdf"), "file_name.pdf")
  expect_equal(sanitize_filename("file<name>.pdf"), "file_name_.pdf")
})


test_that("sanitize_filename does not modify valid filenames", {
  # Mock shiny::showNotification
  local_mocked_bindings(
    showNotification = function(...) {}, # Mock function does nothing
    .package = "shiny"
  )

  # Test cases with no illegal characters
  expect_equal(sanitize_filename("filename.pdf"), "filename.pdf")
  expect_equal(sanitize_filename("file_name.pdf"), "file_name.pdf")
})

test_that("sanitize_filename handles edge cases", {
  # Mock shiny::showNotification
  local_mocked_bindings(
    showNotification = function(...) {}, # Mock function does nothing
    .package = "shiny"
  )

  # Test cases with empty strings and special cases
  expect_equal(sanitize_filename(""), "")
  expect_equal(sanitize_filename("::::"), "____")
  expect_equal(sanitize_filename("file<>name"), "file__name")
})
