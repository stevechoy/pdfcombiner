#' Parse Pages of a PDF
#'
#' @param input_string String containing pages to remove
#'
#' @return a numeric vector containing pages to remove
#' @export
parse_pages_to_remove <- function(input_string) {
  parts <- unlist(strsplit(input_string, ",")) # Split by commas
  pages <- unlist(lapply(parts, function(part) {
    if (grepl("-", part)) { # Handle ranges (e.g., "5-10")
      range <- as.numeric(unlist(strsplit(part, "-")))
      if (length(range) == 2 && !any(is.na(range))) seq(range[1], range[2])
    } else { # Handle single page numbers
      as.numeric(part)
    }
  }))
  unique(pages[!is.na(pages)])
}
