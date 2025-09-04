#' @name watermark_stamp
#' @title Watermark Stamp
#'
#' @param input_pdf Path of input PDF
#' @param output_pdf Path of output PDF
#' @param watermark_text Watermark text
#' @param watermark_fontsize Watermark fontsize
#' @param watermark_col Watermark color
#' @param fallback_col Fallback color to use
#' @param watermark_alpha Watermark alpha
#' @param watermark_rot Watermark rotation angle
#' @param watermark_fontface Watermark fontface (one of "plain", "bold", "italic", "bold.italic")
#' @param watermark_height Watermark overlay height (inches)
#' @param watermark_width Watermark overlay width (inches)
#'
#' @returns a path of output PDF
#' @keywords internal
watermark_stamp <- function(input_pdf,
                            output_pdf,
                            watermark_text,
                            watermark_fontsize = 50,
                            watermark_col = "gray80",
                            fallback_col  = "gray80",
                            watermark_alpha = 0.6,
                            watermark_rot   = 45,
                            watermark_fontface = "bold",
                            watermark_height = 11,
                            watermark_width  = 8.5) {

  # Validate the watermark color
  validate_color <- function(color) {
    tryCatch({
      grDevices::col2rgb(color)  # Check if the color is valid
      TRUE  # If no error, the color is valid
    }, error = function(e) {
      FALSE  # If an error occurs, the color is invalid
    })
  }

  if (!validate_color(watermark_col)) {
    if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
      showNotification(paste0("Provided color is not valid. Using the default color (", fallback_col, ") instead."), type = "error")
    }

    watermark_col <- fallback_col
  }

  # Create a temporary PDF with the watermark text
  temp_watermark_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_watermark_pdf, width = watermark_width, height = watermark_height)  # Standard US Letter size, i.e. approximately 216 mm by 279 mm, is different than A4 at 210 mm by 297 mm
  grid::grid.text(
    label = watermark_text,
    x = 0.5, y = 0.5, gp = grid::gpar(fontsize = watermark_fontsize, col = watermark_col, alpha = watermark_alpha, fontface = watermark_fontface),
    rot = watermark_rot  # Rotate the watermark text
  )
  dev.off()

  # Overlay the watermark PDF onto the input PDF (preserves bookmarks)
  #requireNamespace("qpdf") # "qpdf" is an Import from pdftools
  qpdf::pdf_overlay_stamp(
    input = input_pdf,
    stamp = temp_watermark_pdf,
    output = output_pdf
  )

  # Clean up the temporary watermark PDF
  unlink(temp_watermark_pdf)

  # Return the path to the output PDF
  return(output_pdf)
}
