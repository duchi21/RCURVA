#' Probability Density Function of the Curve Distribution
#'
#' Computes the probability density function (PDF) of the Curve
#' distribution for a given set of values.
#'
#' The probability density function is defined as:
#'
#' \deqn{
#' f(x) = \begin{cases}
#' \frac{3(x-1)^2}{8}, & 1 \leq x \leq 3,\\
#' 0, & \text{otherwise}.
#' \end{cases}
#' }
#'
#' @param x Numeric vector of quantiles where the density
#'   function is evaluated.
#'
#' @return A numeric vector containing the corresponding
#'   probability density values.
#'
#' @details
#' The Curve distribution is a continuous probability distribution
#' with support on the interval [1, 3]. The density function is
#' zero outside this interval.
#' @examples
#' dcurve(2)
#' @export

dcurve <- function(x) {
  ifelse(x >= 1 & x <= 3, 3*(x-1)^2/8, 0)
}
