#' Parse Pages of a PDF
#'
#' @param input_string String containing pages to remove
#'
#' @return a numeric vector containing pages to remove
#' @keywords internal
parse_pages_to_remove <- function(input_string) {
  parts <- unlist(strsplit(input_string, ",")) # Split by commas
  pages <- unlist(lapply(parts, function(part) {
    if (grepl("-", part)) { # Handle ranges (e.g., "5-10")
      range <- suppressWarnings(as.numeric(unlist(strsplit(part, "-"))))
      if (length(range) == 2 && all(!is.na(range)) && range[1] <= range[2]) seq(range[1], range[2])
    } else { # Handle single page numbers
      if (!is.na(suppressWarnings(as.numeric(part)))) as.numeric(part)
    }
  }))
  if (is.null(pages)) {
    return(numeric(0)) # Explicitly return an empty numeric vector if no valid pages are found
  }
  unique(pages[!is.na(pages)])
}
