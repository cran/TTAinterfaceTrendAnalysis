\name{seasonTrend}
\alias{seasonTrend}
\title{
  Determine seasonal trends (code modified from the former wq package)
}
\description{
  Finds the trend for each season and each variable in a time series.
}

\usage{
  
  seasonTrend(x, ...)
}

\arguments{
  \item{x}{Time series vector, or time series matrix with column names}
  \item{...}{Further options to pass to plotting function}
}

\value{
  A data frame with the following fields:
  \item{series}{series names}
  \item{season}{season number}
  \item{sen.slope}{Sen slope in original units per year}
  \item{sen.slope.rel}{Sen slope divided by median for that specific season and series}
  \item{p}{p-value for the trend according to the Mann-Kendall test.}
  \item{missing}{Proportion of slopes joining first and last fifths of the data that are missing}
}

\author{
  Alan D. Jassby and James E. Cloern
}


\seealso{
  \code{\link{mannKen}}
}