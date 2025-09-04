#' Check if a Number is a Non-Negative Numeric
#'
#' This internal function checks whether the input is a non-negative numeric scalar.
#' It can optionally throw an error if the input is invalid.
#'
#' @param x The input to check. Should be a single numeric value.
#' @param name_of_func Name of function to return as error message
#' @param throw_error Logical. If `TRUE`, the function throws an error when the input is invalid.
#'   Defaults to `FALSE`, in which case the function returns `FALSE` for invalid inputs.
#'
#' @return A logical value:
#'   - `TRUE` if `x` is a non-negative numeric scalar.
#'   - `FALSE` if `x` is not a non-negative numeric scalar`.
#'
#' @details
#' This function is intended for internal use within the package to validate numeric inputs.
#' It ensures that the input is numeric, has a length of 1, and is greater than 0.
#'
#' @keywords internal
is_non_negative_numeric <- function(x, name_of_func, throw_error = TRUE) {
  if (is.numeric(x) && length(x) == 1 && !is.na(x) && !is.nan(x) && is.finite(x) && x >= 0) {
    return(TRUE)  # Valid numeric
  } else {
    if (throw_error) {
      stop(paste0(name_of_func, " must be a non-negative numeric value."))
    }
    return(FALSE)  # Invalid input
  }
}
