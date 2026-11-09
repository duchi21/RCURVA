#' Quantile Function of the Curve Distribution
#'
#' Computes the quantile function of the Curve distribution
#' for a given set of probabilities.
#'
#' The quantile function is the inverse of the cumulative
#' distribution function and is defined as:
#'
#' \deqn{
#' Q(u) = 1 + \sqrt[3]{8u}, \quad 0 \leq u \leq 1.
#' }
#'
#' @param u Numeric vector of probabilities between 0 and 1.
#'
#' @return A numeric vector of quantiles corresponding
#'   to the supplied probabilities.
#'
#' @details
#' The quantile function transforms probabilities into
#' values of the Curve distribution. It can also be used
#' to generate random observations through the inverse
#' transform sampling method.
#' @examples
#' qcurve(0.5)
#' @export

qcurve <- function(u) {
  (8*u)^(1/3) + 1
}
