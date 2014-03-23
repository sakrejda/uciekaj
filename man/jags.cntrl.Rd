\name{jags.cntrl}
\alias{jags.cntrl}
\title{A single function which runs a BUGS model, taking all the
arguments from a control list.}
\description{The function runs a BUGS model through rjags/jags.run, taking all the arguments from an prepared control list (see \link{read.cntrl},\link{init.cntrl}).}
\usage{
jags.cntrl(cntrl)
}
\arguments{
	\item{cntrl}{Control list with all variables required for jags.run.
	See \link{jags.run}.}
}
\value{Returns a list including the model for further sampling, the sample, and timing of both the adaptive and sampling phases.}
\references{}
\author{Krzysztof Sakrejda}
\seealso{
\code{\link{jags.file},\link{jags.cntrl},\link{jags.run}}
}
\keyword{~JAGS}
\keyword{~BUGS}

