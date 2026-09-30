#' Convert a PDF to PNG images
#'
#' Renders each page of a PDF as a PNG. Must be called inside a Shiny
#' `withProgress()` block because it updates the progress bar.
#'
#' @param pdf_path Path to the input PDF.
#' @param output_dir Directory in which a temporary working folder is created.
#' @param dpi Rendering resolution in dots per inch. Default is 300.
#'
#' @return Path to the output file: the PNG itself if the PDF has one page,
#'   otherwise a ZIP archive containing one PNG per page.
#'   Errors if no pages are converted.
#' @keywords internal
convert_to_images <- function(pdf_path, output_dir, dpi = 300) {
  # Fresh, empty working folder for this conversion only, so nothing from an
  # earlier conversion in the same session (old zip, old pages) can leak in
  work_dir <- tempfile(pattern = "pngs_", tmpdir = output_dir)
  dir.create(work_dir)

  # Get the total number of pages without rasterizing anything
  total_pages <- pdftools::pdf_info(pdf_path)$pages

  # Initialize a vector to store the paths of the PNG files
  png_files <- character(0)

  # Render one page at a time so only a single page is held in the pixel cache
  for (i in seq_len(total_pages)) {
    shiny::setProgress(value = 0.3 + 0.4 * i / total_pages,
                detail = paste0("Saving page ", i, "/", total_pages))

    # Read just this page from the PDF
    page_image <- magick::image_read_pdf(pdf_path, pages = i, density = dpi)

    # Define the output file path and save the page as a PNG file
    png_path <- file.path(work_dir, paste0("page_", i, ".png"))
    magick::image_write(page_image, path = png_path, format = "png")
    png_files <- c(png_files, png_path)

    # Release the page before reading the next one
    rm(page_image)
    gc()
  }

  if (length(png_files) > 1) {
    # Multiple pages: compress into a ZIP file
    zip_path <- file.path(work_dir, "images.zip")
    shiny::setProgress(value = 0.7, detail = "Compressing into .zip")

    # Zip from inside work_dir using bare file names, so the archive
    # contains the .png files at the top level (no folder structure)
    old_wd <- setwd(work_dir)
    on.exit(setwd(old_wd), add = TRUE)
    utils::zip(zipfile = zip_path, files = basename(png_files))

    return(zip_path)
  } else if (length(png_files) == 1) {
    # Single page: return the PNG directly, no zip
    return(png_files[1])
  } else {
    stop("No pages were successfully converted to images.")
  }
}
