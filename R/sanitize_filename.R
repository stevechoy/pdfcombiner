#' Sanitize File names
#'
#' @param filename String containing a user-provided file name
#'
#' @return a sanitized character for the file name
#' @keywords internal
sanitize_filename <- function(filename) {
  # Define illegal characters
  illegal_chars <- "[\\\\/:*?\"<>|]"

  # Check if the filename contains illegal characters
  if (grepl(illegal_chars, filename)) {
    # Remove illegal characters from the filename
    sanitized_filename <- gsub(illegal_chars, "_", filename)
    # Notify the user about illegal characters
    shiny::showNotification(paste0("Illegal characters are replaced with underscores (new filename: ", sanitized_filename, ".pdf)."),
                            type = "warning", duration = 12)
    return(sanitized_filename)
  }

  # If no illegal characters, return the original filename
  return(filename)
}
