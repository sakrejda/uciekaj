\name{init.cntrl}
\alias{init.cntrl}
\title{A function to initialize a control list for running JAGS models.}
\description{This function loads the data files and runs the adapter script.}
\usage{
init.cntrl(cntrl, reinit=FALSE)
}
\arguments{
	\item{cntrl}{A control list created by read.cntrl, initialized or not.}
	\item{reinit}{A logical variable indicating whether the list should be
	reinitialized.}
}
\value{A control list with the data loaded.}
\references{}
\author{Krzysztof Sakrejda}
\seealso{
\code{\link{read.cntrl}}
}

\keyword{~JAGS}
\keyword{~BUGS}
