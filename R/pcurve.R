#' Cumulative Distribution Function of the Curve Distribution
#'
#' Computes the cumulative distribution function (CDF) of the
#' Curve distribution.
#'
#' The cumulative distribution function is defined as:
#'
#' \deqn{
#' F(x) = \begin{cases}
#' 0, & x < 1,\\
#' \frac{(x-1)^3}{8}, & 1 \leq x \leq 3,\\
#' 1, & x > 3.
#' \end{cases}
#' }
#'
#' @param x Numeric vector of quantiles where the cumulative
#'   distribution function is evaluated.
#'
#' @return A numeric vector of cumulative probabilities
#'   between 0 and 1.
#'
#' @details
#' The function calculates the probability that a random
#' variable following the Curve distribution takes a value
#' less than or equal to x.
#'
#' In other words, it evaluates P(X <= x).
#'
#' @examples
#' pcurve(2)
#' @export

pcurve <- function(x) {
  ifelse(x < 1, 0, ifelse(x <= 3, (x-1)^3/8, 1))
}

