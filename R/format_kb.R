#' Format a file size in KB
#'
#' Rounds to 0 decimal places and adds comma separators.
#'
#' @param x Numeric size in kilobytes.
#'
#' @return A character string, e.g. `format_kb(1131.987)` returns `"1,132"`.
#' @keywords internal
format_kb <- function(x) {
  formatC(x, format = "f", digits = 0, big.mark = ",")
}