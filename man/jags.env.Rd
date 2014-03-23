\name{jags.env}
\alias{jags.env}
\title{A single function which runs a BUGS model, taking all the
arguments form an environemnt.}
\description{The function runs a BUGS model through rjags/jags.run,
taking all the arguments from an environment.}
\usage{
jags.env(env)
}
\arguments{
	\item{env}{Environment with all variables required for jags.run.  See \link{jags.run}.}
}
\value{Returns a list including the model for further sampling, the sample, and timing of both the adaptive and sampling phases.}
\references{}
\author{Krzysztof Sakrejda}
\seealso{
\code{\link{jags.file},\link{jags.cntrl},\link{jags.run}}
}
\keyword{~JAGS}
\keyword{~BUGS}

