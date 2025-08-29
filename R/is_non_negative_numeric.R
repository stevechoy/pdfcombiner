#' Check if a Number is a Non-Negative Numeric
#'
#' This internal function checks whether the input is a non-negative numeric scalar.
#' It can optionally throw an error if the input is invalid.
#'
#' @param x The input to check. Should be a single numeric value.
#' @param throw_error Logical. If `TRUE`, the function throws an error when the input is invalid.
#'   Defaults to `FALSE`, in which case the function returns `FALSE` for invalid inputs.
#'
#' @return A logical value:
#'   - `TRUE` if `x` is a non-negative numeric scalar.
#'   - `FALSE` if `x` is not a non-negative numeric scalar and `throw_error = FALSE`.
#'
#' @details
#' This function is intended for internal use within the package to validate numeric inputs.
#' It ensures that the input is numeric, has a length of 1, and is greater than 0.
#'
#' @examples
#' # Valid input
#' is_non_negative_numeric(10)  # Returns TRUE
#' is_non_negative_numeric(0)  # Returns TRUE
#'
#' # Invalid inputs
#' is_non_negative_numeric(-5)  # Returns FALSE
#' is_non_negative_numeric("ten")  # Returns FALSE
#' is_non_negative_numeric(c(1, 2))  # Returns FALSE
#'
#' # Throwing an error
#' \dontrun{
#' is_non_negative_numeric(-5, throw_error = TRUE)  # Throws an error
#' }
#'
#' @keywords internal
is_non_negative_numeric <- function(x, throw_error = TRUE) {
  if (is.numeric(x) && length(x) == 1 && x >= 0) {
    return(TRUE)  # Valid numeric
  } else {
    if (throw_error) {
      stop("Error: The input must be a non-negative numeric value.")
    }
    return(FALSE)  # Invalid input
  }
}
