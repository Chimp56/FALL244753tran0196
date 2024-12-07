% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myboot2.R
\name{myboot2}
\alias{myboot2}
\title{myboot2}
\usage{
myboot2(
  iter = 10000,
  x,
  fun = "mean",
  alpha = 0.05,
  cx = 1.5,
  xlabel = "mean",
  ...
)
}
\arguments{
\item{iter}{Number of bootstrap samples}

\item{x}{Data}

\item{fun}{Function to apply to the bootstrap samples}

\item{alpha}{Significance level}

\item{cx}{Size of the confidence interval text}

\item{xlabel}{Label for the x-axis}

\item{...}{Additional arguments to pass to \code{hist}}
}
\value{
A histogram of the bootstrap sample statistics with a confidence interval
}
\description{
Bootstrap Confidence Intervals for a Sample Statistic
}
\examples{
x=rnorm(100)
myboot2(10000,x,fun="mean",alpha=0.05,cx=1.5)
obj = myboot2(iter=10000, x, fun = "mynew", alpha = 0.05, cx = 1.5, breaks = 20, col = "blue")
}
