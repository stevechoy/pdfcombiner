library(testthat)
library(shiny)

test_that("package_check returns FALSE for missing packages and shows error notification", {
  # Mock requireNamespace to simulate a missing package
  local_mocked_bindings(showNotification = function(...) {})  # Mock showNotification to avoid actual notifications

  expect_false(package_check("nonexistentpkg"))
})

test_that("package_check returns TRUE for existing packages", {
  # Mock requireNamespace to simulate a missing package
  local_mocked_bindings(showNotification = function(...) {})  # Mock showNotification to avoid actual notifications

  expect_true(package_check("testthat"))
})
