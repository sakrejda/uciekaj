\name{jags.file}
\alias{jags.file}
\title{A single function which runs a BUGS model, taking all the
arguments from a control file in YAML format.}
\description{The function runs a BUGS model through rjags/jags.run,
taking all the arguments from a file in YAML format.}
\usage{
jags.file(file)
}
\arguments{
	\item{file}{File in YAML format with elements for data, inits, etc...
	(see below)}
}
\details{The file format is as follows:
\itemize{
  \item [name] Model name.
	\item [model.file] Path to the BUGS file which defines the model.
	\item [data.dir] Path to the directory which contains data files.
	\item [data.files] Names of the data files to load (using read.table)
	\item [inits.files] Names of the initial value files to load (using read.table)
	\item [adapter.file] Name of the file which contains functions to turn data and initial values into rjags-ready "final.data" and "final.inits" fields.
	\item [n.chains] Number of chains to run.
	\item [n.adapt] Number of adaptive steps to run.
	\item [n.samples] Number of samples desired.
	\item [n.iter] Number of iterations to take, this determines thinning.
	\item [monitor] Names of the variables to monitor.
}
}
\value{Returns a list including the model for further sampling, the sample, and timing of both the adaptive and sampling phases.}
\references{}
\author{Krzysztof Sakrejda}
\seealso{
\code{\link{jags.run},\link{jags.cntrl},\link{jags.run}}
}
\keyword{~JAGS}
\keyword{~BUGS}

