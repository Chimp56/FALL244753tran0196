#' myci
#'
#' @description
#' Compute a confidence interval for the mean of a normal distribution
#'
#' @param data a numeric vector of data
#' @param alpha the confidence level, default is 0.05
#'
#' @return a vector of length 2 with the lower and upper bounds of the confidence interval
#' @export
#' @importFrom stats qt sd
#'
#' @examples
#' myci(rnorm(100))
myci <- function(data, alpha=0.05) {
  mu <- mean(data)
  s <- sd(data)
  n <- length(data)
  t <- qt(1 - alpha/ 2, df = n - 1)
  lower <- mu - t * s / sqrt(n)
  upper <- mu + t * s / sqrt(n)
  c(lower, upper)
}
