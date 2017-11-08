## A simple annotated function ----

#' Calculate a percentage
#'
#' \code{calculate_percentage} calculates a ratio and express it as a percentage
#'
#' @param x A number
#' @param y A number
#' @param d An integer
#' @return The ratio of x/y expressed as a percentage with d digits
#' @export
#' @examples
#' calculate_percentage(27, 80, 1)
#'
calculate_percentage <- function(x, y, d) {
  decimal <- x / y  # Calculate decimal value
  round(100 * decimal, d)  # Convert to % and round to d digits
}
