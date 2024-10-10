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
#' @return plots of Objective function Vs n
#'
#' @importFrom stats qbinom pbinom pnorm uniroot
#' @importFrom graphics abline par
#' @export
#'
#' @examples
#' ntickets(100,0.1,0.9)
#' ntickets(N=400,gamma = 0.02, p = 0.95)
#' ntickets(N=200,gamma = 0.02, p = 0.95)
ntickets <- function(N,gamma,p){
  # Discrete
  n <- seq(N, floor(N + N/10), by = 1)
  discrete_objective <- 1 - gamma - pbinom(N, size = n, prob = p)

  index <- which.min(abs(discrete_objective))
  nd <- n[index]

  # Continuous Approach (Normal Approximation)
  mean_approx <- N * p
  sd_approx <- sqrt(N * p * (1 - p))
  objective_normal <- function(n) {
    1 - gamma - pnorm(N + 0.5, mean = n * p, sd = sqrt(n * p * (1 - p)))
  }

  # Find nc by solving the equation 1 - gamma = pnorm(...)
  nc <- uniroot(function(n) objective_normal(n), lower = N, upper = N + 50)$root

  # Create the plot for the normal approximation
  continuous_objective <- 1 - gamma - pnorm(N, mean = n * p, sd = sqrt(n * p * (1 - p)))

  # Plot for discrete case
  par(mfrow=c(2,1)) # Set up a 2-row layout for plots

  plot(n, discrete_objective, type = "b", pch = 20, col = "black",
       main = paste0("Objective Vs n to find optimal tickets sold\n(", round(nd, 4),
                     ") gamma= ", gamma, " N=", N, " discrete"),
       xlab = "n", ylab = "Objective")
  abline(h = 0, col = "red")
  abline(v = nd, col = "red", lwd = 2)

  # Plot for continuous case
  plot(n, continuous_objective, type = "l", col = "black",
       main = paste0("Objective Vs n to find optimal tickets sold\n(", round(nc, 4),
                     ") gamma= ", gamma, " N=", N, " continuous"),
       xlab = "n", ylab = "Objective")
  abline(h = 0, col = "blue")
  abline(v = nc, col = "blue", lwd = 2)

  return(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}
