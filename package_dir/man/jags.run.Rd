\name{jags.run}
\alias{jags.run}
\title{A single function which initializes and samples a BUGS model.}
\description{This function initializes and samples from a BUGS model
using (R)JAGS.  Each part is timed and the timings are returned along
with the sample and the model for further samples.}
\usage{
jags.run(
    file = '',
    data = NULL,
    inits = NULL,
    variable.names = NULL,
    n.chains = 1,
    n.adapt = 100,
    n.iter = 100,
    n.samples = 100
)
}
\arguments{
	\item{file}{A BUGS language model description.}
  \item{data}{A named list of data elements.}
	\item{inits}{A named list of initialization values.}
	\item{variable.names}{A character vector of variable names to monitor.}
	\item{n.chains}{The number of chains to run.}
	\item{n.adapt}{The number of adaptive iterations to run.}
	\item{n.iter}{The number of iterations to run.}
	\item{n.samples}{The number of samples to keep (determines thinning).}
}
\value{Returns a list including the model for further sampling, the sample, and timing of both the adaptive and sampling phases.}
\references{}
\author{Krzysztof Sakrejda}
\seealso{
\code{\link{jags.file},\link{jags.cntrl},\link{jags.env}}
}
\keyword{~JAGS}
\keyword{~BUGS}

