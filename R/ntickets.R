#' ntickets
#'
#' calculates the number of tickets to be sold when the number of seats in the
#' flight is N and the probability of a "show" is p and gamma is the probability
#' a plane will be truly overbooked (more people show than there are seats)
#'  Use solely the appropriate discrete distribution
#'  Use the normal approximation
#'
#' prints a named list containing nd, nc, N, p and gamma - where nd is
#' calculated using the discrete distribution and nc is the same calculated with
#' normal approximation.
#'
#' creates a plot of Objective function Vs n, the objective function can be
#' constructed from the defining equation by simply making the equation equal
#' zero -- example 1-gamma-pnorm(...) = 0. The left-hand side can be called an
#' objective function. Make two of these -- one for the discrete and one for the
#' continuous case.
#'
#'
#' @param N number of seats in the flight
#' @param gamma probability a plane will be truly overbooked
#' @param p probability of a "show"
#'
#' @return a named list containing nd, nc, N, p and gamma
#'
#' @export
#'
#' @examples
#' ntickets(100,0.1,0.9)
#' ntickets(N=400,gamma = 0.02, p = 0.95)
#' ntickets(N=200,gamma = 0.02, p = 0.95)
ntickets <- function(N,gamma,p){
  # Discrete
  nd <- qbinom(1-gamma, N, p)

  # Normal
  nc <- qnorm(1-gamma, mean = N*p, sd = sqrt(N*p*(1-p))) - 0.5

  # Plot
  n <- seq(0, N, 1)

  ## plot of Objective function Vs n discrete
  obj_discrete <- 1-gamma-pbinom(n, N, p)
  plot(n, obj_discrete, type = "l", col = "blue", xlab = "n", ylab = "Objective function", main = "Objective function Vs n")

  ## plot of Objective function Vs n continuous
  obj_continuous <- 1-gamma-pnorm(n)
  lines(n, obj_continuous, col = "red")

  return(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}
