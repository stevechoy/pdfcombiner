#' Convert an image to a PDF
#'
#' Flattens any transparency onto a white background and writes the image
#' to a PDF. Multi-frame images (e.g. GIF, TIFF) become one page per frame.
#'
#' @param img_path Path to the input image.
#' @param pdf_path Path for the output PDF.
#' @param dpi Resolution used to set page size (pixels / dpi = inches).
#'   Default is 72.
#'
#' @return The path to the PDF that was written (`pdf_path`).
#' @keywords internal
image_to_pdf <- function(img_path, pdf_path, dpi = 72) {
  frames <- magick::image_read(img_path)          # may have >1 frame (gif/tiff)
  frame_pdfs <- character(length(frames))

  for (k in seq_along(frames)) {
    fr <- magick::image_flatten(
      magick::image_background(frames[k], "white"), "Over")   # remove alpha
    info <- magick::image_info(fr)
    ras  <- grDevices::as.raster(fr)

    frame_pdfs[k] <- tempfile(fileext = ".pdf")
    grDevices::pdf(frame_pdfs[k], width = info$width / dpi, height = info$height / dpi)
    grid::grid.newpage()
    grid::grid.raster(ras, interpolate = FALSE, width = 1, height = 1)
    grDevices::dev.off()
  }

  if (length(frame_pdfs) == 1) {
    file.copy(frame_pdfs, pdf_path, overwrite = TRUE)
  } else {
    qpdf::pdf_combine(frame_pdfs, output = pdf_path)
  }
  pdf_path
}
