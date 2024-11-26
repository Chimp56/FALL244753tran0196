#' @title mynew
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
