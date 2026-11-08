#' Random Number Generation for the Curva Distribution
#'
#' Generates random numbers from the Curva distribution.
#'
#' @param n Number of observations.
#' @return A vector of random numbers.
#' @examples
#' rcurva(10)
#' @export

rcurva <- function(n) {
  u <- stats::runif(n)
  qcurva(u)
}
