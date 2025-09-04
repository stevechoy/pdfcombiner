test_that("pdfcombiner launches without errors", {
  skip_if_not(interactive(), "Skipping test: Not an interactive session")

  expect_error(pdfcombiner(bootstrap_theme = FALSE), NA) # Expect no errors
})

test_that("pdfcombiner handles invalid input gracefully", {
  skip_if_not(interactive(), "Skipping test: Not an interactive session")

  expect_error(pdfcombiner(max_file_size = -1), "max_file_size must be a non-negative numeric value.")
})

test_that("pdfcombiner handles bad bootstrat_theme arguments", {
  skip_if_not(interactive(), "Skipping test: Not an interactive session")

  expect_error(pdfcombiner(bootstrap_theme = "a"), regexp = "bootstrap_theme argument must be a logical \\(TRUE or FALSE\\)\\.")
  expect_error(pdfcombiner(bootstrap_theme = 1), regexp = "bootstrap_theme argument must be a logical \\(TRUE or FALSE\\)\\.")
  expect_error(pdfcombiner(bootstrap_theme = NA), "missing value where TRUE/FALSE needed")
})

test_that("pdfcombiner handles bad defaultwm_fontface arguments", {
  skip_if_not(interactive(), "Skipping test: Not an interactive session")

  expect_error(pdfcombiner(defaultwm_fontface = "123"), "Default_fontface must be one of 'plain', 'bold', 'italic', 'bold.italic'.")
  expect_error(pdfcombiner(defaultwm_fontface = 123), "Default_fontface must be one of 'plain', 'bold', 'italic', 'bold.italic'.")
  expect_error(pdfcombiner(defaultwm_fontface = "PLAIN"), "Default_fontface must be one of 'plain', 'bold', 'italic', 'bold.italic'.")
  expect_error(pdfcombiner(defaultwm_fontface = TRUE), "Default_fontface must be one of 'plain', 'bold', 'italic', 'bold.italic'.")
  expect_error(pdfcombiner(defaultwm_fontface = NA), "Default_fontface must be one of 'plain', 'bold', 'italic', 'bold.italic'.")
})

