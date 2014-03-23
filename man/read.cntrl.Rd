\name{read.cntrl}
\alias{read.cntrl}
\title{A function to read a control file for running BUGS models.}
\description{Loads location of model file, data files, adapter script, and parameters for running the model from a file in YAML format.}
\usage{
read.cntrl(file)
}
\arguments{
	\item{file}{YAML format file with fields for the name of the model,
	the path to the BUGS model file, the path to the directory containing
	data files, the names of the data files to load (using read.table),
	the name of a script defining the functions data_adapter and
	inits_adapter for creating final.data and final.inits in the control
	list, the number of chains, number of adaptive steps, number of
	samples desired, and the number of iterations, as well as a list of
	variables to monitor.}
}
\value{Returns a list, with the added class "jags_cntrl" and a logical
attribute "data" which defines whether the list has been initialized
(data loaded).}
\references{}
\author{Krzysztof Sakrejda}
\seealso{
\code{\link{init.cntrl}}
}

\keyword{~JAGS}
\keyword{~BUGS}

