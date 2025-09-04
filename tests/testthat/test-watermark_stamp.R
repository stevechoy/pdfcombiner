library(testthat)
library(shiny)
library(grid)
library(qpdf)

test_that("watermark_stamp creates an output PDF successfully", {
  # Create a temporary input PDF
  input_pdf <- tempfile(fileext = ".pdf")
  pdf(input_pdf)
  grid::grid.text("Test PDF Content", x = 0.5, y = 0.5)
  dev.off()

  # Define output PDF path
  output_pdf <- tempfile(fileext = ".pdf")

  # Call the function
  result <- watermark_stamp(
    input_pdf = input_pdf,
    output_pdf = output_pdf,
    watermark_text = "CONFIDENTIAL"
  )

  # Check that the output PDF exists
  expect_true(file.exists(result))

  # Clean up
  unlink(input_pdf)
  unlink(output_pdf)
})

test_that("watermark_stamp validates and falls back to default color", {
  # Create a temporary input PDF
  input_pdf <- tempfile(fileext = ".pdf")
  pdf(input_pdf)
  grid::grid.text("Test PDF Content", x = 0.5, y = 0.5)
  dev.off()

  # Define output PDF path
  output_pdf <- tempfile(fileext = ".pdf")

  # Mock `shiny::showNotification` to do nothing
  local_mocked_bindings(
    `showNotification` = function(...) {}#,  # Replace with a no-op
  )
  # Call the function with an invalid color
  result <- watermark_stamp(
    input_pdf = input_pdf,
    output_pdf = output_pdf,
    watermark_text = "CONFIDENTIAL",
    watermark_col = "invalid_color",
    fallback_col = "gray50"
  )

  # Check that the output PDF exists
  expect_true(file.exists(result))

# Clean up
unlink(input_pdf)
unlink(output_pdf)
})
