\name{mannKen}
\alias{mannKen}
\title{
  Mann-Kendall trend test and the Sen slope (code modified from the former wq package)
}
\description{
  Applies Kendall's tau test for the significance of a monotonic time series trend. Also
  calculates the Sen slope as an estimate of this trend.  
}

\usage{
  
  mannKen(x, ...)
}

\arguments{
  \item{x}{A numeric vector, matrix or data frame}
  \item{...}{Other arguments to pass to plotting function}
}

\value{
A list of the following if x is a vector:
\item{sen.slope Sen}{slope}
\item{sen.slope.rel}{Relative Sen slope}
\item{p.value}{Significance of slope}
\item{S}{Kendall's S}
\item{varS}{Variance of S}
\item{miss}{Fraction of missing slopes connecting first and last fifths of x
or a matrix with corresponding columns if x is a matrix or data frame.}
}

\author{
  Alan D. Jassby and James E. Cloern
}


\seealso{
  \code{\link{seaKen}}
  \code{\link{seasonTrend}}
}
