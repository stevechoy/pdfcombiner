#' Convert to Word
#'
#' @param pdf_path Path of input PDF
#' @param output_path Path to save new Word file to
#'
#' @return print statement (as a side effect)
#' @export
convert_to_word <- function(pdf_path, output_path) {
  # Extract text from the PDF
  pdf_text <- pdf_text(pdf_path)

  # Create a Word document
  doc <- officer::read_docx()

  # Add extracted text to the Word document
  for (page in seq_along(pdf_text)) {
    doc <- officer::body_add_par(doc, paste0("Page ", page), style = "heading 1")
    doc <- officer::body_add_par(doc, pdf_text[page], style = "Normal")
  }

  # Save the Word document
  print(doc, target = output_path)
}
