library(testthat)

test_that("is_non_negative_numeric works for valid inputs", {
  # Valid non-negative numeric inputs
  expect_true(is_non_negative_numeric(0, "test_func"))
  expect_true(is_non_negative_numeric(1, "test_func"))
  expect_true(is_non_negative_numeric(3.14, "test_func"))
})

test_that("is_non_negative_numeric returns FALSE for invalid inputs when throw_error = FALSE", {
  # Invalid inputs with throw_error = FALSE
  expect_false(is_non_negative_numeric(-1, "test_func", throw_error = FALSE))  # Negative number
  expect_false(is_non_negative_numeric("string", "test_func", throw_error = FALSE))  # Non-numeric
  expect_false(is_non_negative_numeric(c(1, 2), "test_func", throw_error = FALSE))  # Vector
  expect_false(is_non_negative_numeric(NA, "test_func", throw_error = FALSE))  # NA value
  expect_false(is_non_negative_numeric(NULL, "test_func", throw_error = FALSE))  # NULL value
})

test_that("is_non_negative_numeric throws an error for invalid inputs when throw_error = TRUE", {
  # Invalid inputs with throw_error = TRUE
  expect_error(is_non_negative_numeric(-1, "test_func"), "test_func must be a non-negative numeric value.")
  expect_error(is_non_negative_numeric("string", "test_func"), "test_func must be a non-negative numeric value.")
  expect_error(is_non_negative_numeric(c(1, 2), "test_func"), "test_func must be a non-negative numeric value.")
  expect_error(is_non_negative_numeric(NA, "test_func"), "test_func must be a non-negative numeric value.")
  expect_error(is_non_negative_numeric(NULL, "test_func"), "test_func must be a non-negative numeric value.")
})

test_that("is_non_negative_numeric handles edge cases", {
  # Edge cases
  expect_false(is_non_negative_numeric(TRUE, "test_func", throw_error = FALSE))  # Logical value
  expect_false(is_non_negative_numeric(FALSE, "test_func", throw_error = FALSE)) # Logical value
  expect_false(is_non_negative_numeric(Inf, "test_func", throw_error = FALSE))   # Infinite value
  expect_false(is_non_negative_numeric(-Inf, "test_func", throw_error = FALSE)) # Negative infinite value
  expect_false(is_non_negative_numeric(NaN, "test_func", throw_error = FALSE))  # NaN value
})
