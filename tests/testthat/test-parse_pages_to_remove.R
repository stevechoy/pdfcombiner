library(testthat)

test_that("parse_pages_to_remove handles single page numbers correctly", {
  # Single page numbers
  expect_equal(parse_pages_to_remove("1"), c(1))
  expect_equal(parse_pages_to_remove("5"), c(5))
  expect_equal(parse_pages_to_remove("10"), c(10))
})

test_that("parse_pages_to_remove handles page ranges correctly", {
  # Page ranges
  expect_equal(parse_pages_to_remove("1-3"), c(1, 2, 3))
  expect_equal(parse_pages_to_remove("5-7"), c(5, 6, 7))
  expect_equal(parse_pages_to_remove("10-12"), c(10, 11, 12))
})

test_that("parse_pages_to_remove handles mixed single pages and ranges", {
  # Mixed single pages and ranges
  expect_equal(parse_pages_to_remove("1,3-5,7"), c(1, 3, 4, 5, 7))
  expect_equal(parse_pages_to_remove("2,4-6,8,10-12"), c(2, 4, 5, 6, 8, 10, 11, 12))
})

test_that("parse_pages_to_remove removes duplicates", {
  # Duplicate pages
  expect_equal(parse_pages_to_remove("1,1,2,2,3-5,3-5"), c(1, 2, 3, 4, 5))
})

test_that("parse_pages_to_remove handles invalid input gracefully", {
  # Invalid input
  expect_equal(parse_pages_to_remove("a,b,c"), numeric(0)) # Non-numeric input
  expect_equal(parse_pages_to_remove("1,a,3-5"), c(1, 3, 4, 5)) # Mixed valid and invalid input
  expect_equal(parse_pages_to_remove("1-3,a-5"), c(1, 2, 3)) # Invalid range
})

test_that("parse_pages_to_remove handles edge cases", {
  # Edge cases
  expect_equal(parse_pages_to_remove(""), numeric(0)) # Empty string
  expect_equal(parse_pages_to_remove("1-1"), c(1)) # Single page range
  expect_equal(parse_pages_to_remove("1,1-1"), c(1)) # Single page with duplicate range
  expect_equal(parse_pages_to_remove("1,,2"), c(1, 2)) # Extra commas
  expect_equal(parse_pages_to_remove("1-3,5-3"), c(1, 2, 3)) # Invalid descending range
})
