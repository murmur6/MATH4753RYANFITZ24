% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myboot2.R
\name{myboot2}
\alias{myboot2}
\title{myboot2}
\usage{
myboot2(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...)
}
\arguments{
\item{iter}{number of iterations}

\item{x}{the sample}

\item{fun}{The function to be applied}

\item{alpha}{confidence interval}

\item{cx}{text size}

\item{...}{more arguments}
}
\value{
The graph and a list containing the confidence interval, applied function, and the original sample
}
\description{
myboot2
}
\examples{
myboot2(iter = 10000, x = rnorm(100), fun = "sd", alpha = 0.20)
}
