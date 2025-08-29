#' Convert to Images
#'
#' @param pdf_path Path of input PDF
#' @param output_dir Path to save new PowerPoint file to
#' @param dpi dots per inch for .png files, default 300
#'
#' @return a path to either a .zip file (if multiple images) or a single .png file
#' @export
convert_to_images <- function(pdf_path, output_dir, dpi = 300) {
  # Read the PDF as an image object
  setProgress(value = 0.3, detail = paste0("Reading as images"))
  pdf_images <- magick::image_read_pdf(pdf_path, density = dpi)

  # Get the total number of pages
  total_pages <- length(pdf_images)

  # Initialize a vector to store the paths of the PNG files
  png_files <- c()

  # Loop through each page and save it as a PNG file
  for (i in seq_len(total_pages)) {
    setProgress(value = 0.5, detail = paste0("Saving page ", i , "/", total_pages))
    # Extract the current page as an image
    page_image <- pdf_images[i]

    # Define the output file path
    png_path <- file.path(output_dir, paste0("page_", i, ".png"))

    # Save the image as a PNG file
    magick::image_write(page_image, path = png_path, format = "png")
    png_files <- c(png_files, png_path)
  }

  # If there are multiple pages, compress them into a ZIP file
  if (length(png_files) > 1) {
    zip_path <- file.path(output_dir, "images.zip")
    setProgress(value = 0.7, detail = paste0("Compressing into .zip"))
    utils::zip(zipfile = zip_path, files = png_files)
    return(zip_path)
  } else if (length(png_files) == 1) {
    return(png_files[1])  # Return single PNG file if only one page
  } else {
    stop("No pages were successfully converted to images.")
  }
}
