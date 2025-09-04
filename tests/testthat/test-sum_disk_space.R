library(testthat)

test_that("sum_disk_space calculates total size of existing files", {
  # Create temporary files to simulate PDF files
  temp_file_1 <- tempfile(fileext = ".pdf")
  temp_file_2 <- tempfile(fileext = ".pdf")
  temp_file_3 <- tempfile(fileext = ".pdf")

  # Write some content to the files to give them a size
  writeBin(rep(0, 1024), temp_file_1)  # 1 KB
  writeBin(rep(0, 2048), temp_file_2)  # 2 KB
  # Leave temp_file_3 as non-existent to test the warning

  # Get actual file sizes in kilobytes
  actual_size_1 <- file.info(temp_file_1)$size / 1024
  actual_size_2 <- file.info(temp_file_2)$size / 1024

  # Create a named list of file paths
  file_list <- list(
    "File1" = temp_file_1,
    "File2" = temp_file_2,
    "File3" = temp_file_3  # This file does not exist
  )

  # Call the function
  total_size <- suppressWarnings(sum_disk_space(file_list))

  # Check that the total size is correct (1 KB + 2 KB = 3 KB)
  expect_equal(total_size, actual_size_1 + actual_size_2)

  # Clean up temporary files
  unlink(temp_file_1)
  unlink(temp_file_2)
})

test_that("sum_disk_space calculates size for a single file", {
  # Create a single temporary file
  temp_file <- tempfile(fileext = ".pdf")

  # Write content to the file (1 KB)
  writeBin(rep(0, 1024), temp_file)

  # Get the actual file size in kilobytes
  actual_size <- file.info(temp_file)$size / 1024

  # Create a named list with the single file
  file_list <- list("SingleFile" = temp_file)

  # Call the function
  total_size <- sum_disk_space(file_list)

  # Check that the total size matches the actual size
  expect_equal(total_size, actual_size)

  # Clean up temporary file
  unlink(temp_file)
})

test_that("sum_disk_space returns 0 for an empty file list", {
  # Empty file list
  file_list <- list()

  # Call the function
  total_size <- sum_disk_space(file_list)

  # Check that the total size is 0
  expect_equal(total_size, 0)
})

test_that("sum_disk_space returns 0 for all non-existent files", {
  # Create a list of non-existent files
  file_list <- list(
    "NonExistent1" = tempfile(fileext = ".pdf"),
    "NonExistent2" = tempfile(fileext = ".pdf")
  )

  # Call the function
  total_size <- suppressWarnings(sum_disk_space(file_list))

  # Check that the total size is 0
  expect_equal(total_size, 0)
})

test_that("sum_disk_space handles invalid file paths gracefully", {
  # Create a list with invalid file paths
  file_list <- list(
    "InvalidPath1" = "nonexistent/path/to/file1.pdf",
    "InvalidPath2" = "another/nonexistent/path/to/file2.pdf"
  )

  # Call the function
  total_size <- suppressWarnings(sum_disk_space(file_list))

  # Check that the total size is 0
  expect_equal(total_size, 0)
})
