#' Convert to Excel
#'
#' @param pdf_path Path of input PDF
#' @param output_path Path to save new Excel file to
#'
#' @return a Excel workbook (as a side effect)
#' @keywords internal
convert_to_excel <- function(pdf_path, output_path) {
  # Extract text from the PDF
  pdf_text <- pdf_text(pdf_path)

  # Create an Excel workbook
  wb <- openxlsx::createWorkbook()

  # Add each page's text to a separate sheet
  for (page in seq_along(pdf_text)) {
    openxlsx::addWorksheet(wb, paste0("Page ", page))
    openxlsx::writeData(wb, paste0("Page ", page), pdf_text[page])
  }

  # Save the Excel workbook
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
}
