#' Convert to PowerPoint
#'
#' @param pdf_path Path of input PDF
#' @param output_path Path to save new PowerPoint file to
#'
#' @return a print statement (as a side effect)
#' @keywords internal
convert_to_powerpoint <- function(pdf_path, output_path) {
  # Extract text from the PDF
  pdf_text <- pdf_text(pdf_path)

  # Create a PowerPoint presentation
  ppt <- officer::read_pptx()

  # Add each page's text to a separate slide
  for (page in seq_along(pdf_text)) {
    ppt <- officer::add_slide(ppt, layout = "Title and Content", master = "Office Theme")
    ppt <- officer::ph_with(ppt, paste0("Page ", page), location = officer::ph_location_type(type = "title"))
    ppt <- officer::ph_with(ppt, pdf_text[page], location = officer::ph_location_type(type = "body"))
  }

  # Save the PowerPoint presentation
  print(ppt, target = output_path)
}
