\name{fixdata}
\alias{fixdata}
\title{
Fixdata function
}
\description{
Simply modify your datase through the interface
}
\usage{
fixdata()
}

\value{
The edited database that is automaticaly read by the interface to replace former values
}

\note{
fixdata() call the function fix() that act on the rawdata base. The fix() function itself call the function edit() from the package <utils>
}

\seealso{
\code{\link[utils]{fix}}
\code{\link[utils]{edit}}
}


