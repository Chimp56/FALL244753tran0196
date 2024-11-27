#' @title mynew
#'
#' @description A simple function that computes the cube of the mean of a vector divided by the variance of the vector
#'
#' @param x A numeric vector
#'
#' @return A numeric value
#' @export
#' @importFrom stats var
#'
#' @examples
#' x=rnorm(100)
#' mynew(x)
mynew <- function(x) {
  mean(x)^3/var(x)
}
