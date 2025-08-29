#' @name sum_disk_space
#' @title Sums up disk spaces used for PDF files
#'
#' @param file_list Names list of PDF files
#'
#' @returns A character containing total size in kilobytes
#' @keywords internal
sum_disk_space <- function(file_list) {
  # Initialize total size
  total_size_kb <- 0

  # Iterate over the files in the list
  for (file_name in names(file_list)) {
    file_path <- file_list[[file_name]]

    # Check if the file exists
    if (file.exists(file_path)) {
      # Get the file size in bytes and convert to kilobytes
      file_size_kb <- file.info(file_path)$size / 1024
      total_size_kb <- total_size_kb + file_size_kb
    } else {
      warning(sprintf("File '%s' does not exist at path '%s'. Skipping.", file_name, file_path))
    }
  }

  # Return the total size in kilobytes
  return(total_size_kb)
}
