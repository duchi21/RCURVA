#' Quantile Function of the Curva Distribution
#'
#' Calculates the quantiles of the Curva distribution.
#'
#' @param u Numeric vector of probabilities.
#' @return Quantile values.
#' @examples
#' qcurva(0.5)
#' @export

qcurva <- function(u) {
  (8*u)^(1/3) + 1
}
