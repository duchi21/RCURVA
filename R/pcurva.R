#' Cumulative Distribution Function of the Curva Distribution
#'
#' Calculates the cumulative probability.
#'
#' @param x Numeric vector.
#' @return Cumulative probabilities.
#' @examples
#' pcurva(2)
#' @export

pcurva <- function(x) {
  ifelse(x < 1, 0, ifelse(x <= 3, (x-1)^3/8, 1))
}
