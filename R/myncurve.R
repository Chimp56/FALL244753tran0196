#' @title Normal Curve with Area Calculation
#'
#' @param mu The mean of the normal distribution
#' @param sigma The standard deviation of the normal distribution
#' @param a The value at which to calculate the probability P(X <= a)
#'
#' @return A list containing the mean, standard deviation, and calculated probability
#'
#' @importFrom graphics curve polygon text
#' @importFrom stats dnorm pnorm
#' @export myncurve
#'
#' @examples
#' myncurve(0, 1, 1.5)
myncurve = function(mu, sigma, a) {
  x <- NULL
  # Plot the normal curve
  curve(dnorm(x, mean=mu, sd=sigma),
        xlim = c(mu - 3*sigma, mu + 3*sigma),
        main = paste("Normal Curve with mean =", mu, "and sd =", sigma))

  # Shade the area between the curve and the x-axis up to x=a
  x_values = seq(mu - 3*sigma, a, length=1000)
  y_values = dnorm(x_values, mean=mu, sd=sigma)
  polygon(c(x_values, a), c(y_values, 0), col="lightblue")

  # Calculate the probability P(X <= a)
  prob = pnorm(a, mean=mu, sd=sigma)

  # Display the area (probability)
  mid_y = dnorm(a, mean=mu, sd=sigma)
  text(a, mid_y, round(prob, 4), pos = 4, col = "blue")

  # Return the mean, standard deviation, and calculated probability as a list
  return(list(mu = mu, sigma = sigma, area = prob))
}

