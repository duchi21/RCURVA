#' Chi-Square Goodness-of-Fit Test for the Curve Distribution
#'
#' Performs a Chi-square goodness-of-fit test to determine
#' whether a sample follows the Curve distribution.
#'
#' @param x Numeric vector containing the sample.
#' @param k Number of intervals.
#'
#' @return A list containing the Chi-square statistic (chi),
#' degrees of freedom (df) and p-value (pvalor).
#'
#' @examples
#' x <- rcurve(100)
#' gof_curve(x)
#'
#' @export

gof_curve <- function(x, k = 5) {

  cortes <- qcurve(seq(0, 1, length.out = k + 1))

  observado <- table(cut(x, cortes, include.lowest = TRUE))

  esperado <- length(x) / k

  chi <- sum((observado - esperado)^2 / esperado)

  pvalor <- stats::pchisq(chi, df = k - 1, lower.tail = FALSE)

  return(list(chi = chi, df = k-1, pvalor = pvalor))
}
