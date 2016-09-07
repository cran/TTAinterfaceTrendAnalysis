\name{seaKen}
\alias{seaKen}
\title{
  Seasonal and Regional Kendall trend test (code modified from the former wq package)
}
\description{
  Calculates the Seasonal or Regional Kendall test of trend significance, including an estimate of the
  Sen slope. 
}

\usage{
  
  seaKen(x, ...)
}

\arguments{
  \item{x}{A numeric vector, matrix or data frame made up of seasonal time series}
  \item{...}{Other arguments to pass to plotting function}
}

\value{
  A list of the following if x is a vector: seaKen returns a list with the following members:
  \item{sen.slope}{Sen slope}
  \item{sen.slope.pct}{Sen slope as percent of mean}
  \item{p.value}{significance of slope}
  \item{miss}{for each season, the fraction missing of slopes connecting first and last 20 percent of
  the years or a matrix with corresponding columns if x is a matrix or data frame.}
}

\author{
  Alan D. Jassby and James E. Cloern
}


\seealso{
  \code{\link{mannKen}}
}