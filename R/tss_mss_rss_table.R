#' tss_mss_rss_table
#'
#' Compute the total sum of squares, model sum of squares, and residual sum of squares.
#'
#' @param observed_values A numeric vector of observed values.
#' @param predicted_values A numeric vector of predicted values.
#'
#' @return A data frame with the total sum of squares, model sum of squares, and residual sum of squares.
#' @export
#'
#' @examples
#' observed_values <- c(1, 2, 3, 4, 5)
#' predicted_values <- c(1.5, 2.5, 3.5, 4.5, 5.5)
#' tss_mss_rss_table(observed_values, predicted_values)
#'
tss_mss_rss_table <- function(observed_values, predicted_values) {
  # Compute the total sum of squares
  tss <- sum((observed_values - mean(observed_values))^2)

  # Compute the model sum of squares
  mss <- sum((predicted_values - mean(observed_values))^2)

  # Compute the residual sum of squares
  rss <- sum((observed_values - predicted_values)^2)

  # Create a data frame with the results
  table <- data.frame(tss = tss, mss = mss, rss = rss)

  return(table)
}
