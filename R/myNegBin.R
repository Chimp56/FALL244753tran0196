#' myNegBin
#'
#' @description
#' Compute the probability of y failures before r successes
#'
#' @param y number of failures
#' @param r size
#' @param p probability
#'
#' @return the probability of y failures before r successes
#' @export
#'
#' @examples
#' myNegBin(5,2,0.5)
myNegBin=function(y,r,p){
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}
