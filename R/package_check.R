#' @name package_check
#' @title Package check
#' @param pkg_name Package name in characters
#' @param bookmarks Default FALSE. Whether to show shiny notification for bookmarks
#' @param silent Default FALSE. Whether to show shiny notification
#'
#' @return a logical.
#' @import shiny
#' @import qpdf
#' @import pdftools
#' @importFrom grDevices dev.off pdf
#' @export
package_check <- function(pkg_name, bookmarks = FALSE, silent = FALSE) {
  if (!requireNamespace(pkg_name, quietly = TRUE)) {
    if(!silent) {
      if(bookmarks) {
        showNotification(
          paste0("The '", pkg_name, "' package is not installed. Please install it to retain bookmarks."),
          type = "warning"
        )
      } else {
        showNotification(
          paste0("The '", pkg_name, "' package is not installed. Please install it to use this feature."),
          type = "error"
        )
      }
    }
    return(FALSE)
  } else {
    return(TRUE)
  }
}
