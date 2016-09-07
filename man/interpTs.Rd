\name{interpTs}
\alias{interpTs}
\title{
Interpolate or substitute missing time series values (code of the former wq package)
}
\description{
Imterpolates or substitutes missing data in a time series for gaps up to a specified size.  
}

\usage{
interpTs(x, type = c("linear", "series.median", "series.mean", "cycle.median",
"cycle.mean"), gap = NULL)
}

\arguments{
\item{x}{object of class "ts" or "mts"}
\item{type}{method of interpolation or substitution}
\item{gap}{maximum gap to be replaced}
}

\value{
The time series with some or all missing values replaced.
}

\author{
Alan D. Jassby and James E. Cloern
}


