#' Density Function of the Curva Distribution
#'
#' Calculates the density of the Curva distribution.
#'
#' @param x Numeric vector.
#' @return Density values.
#' @examples
#' dcurva(2)
#' @export

dcurva <- function(x) {
  ifelse(x >= 1 & x <= 3, 3*(x-1)^2/8, 0)
}
