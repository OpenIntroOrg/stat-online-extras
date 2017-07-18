\name{timeDepCoxph}
\alias{timeDepCoxph}
\title{
Cox proportional hazards model for time-dependent covariate
}
\description{
The \code{timeDepCoxph} function is used to create a time-dependent covariate \code{Z} of the form \code{Z[i]*transform(t[i])} for time \code{t[i]}.
}
\usage{
timeDepCoxph(x, colTime, colDelta, colCov, tdCov, transformation=log,
	outputModel=TRUE, outputDF=FALSE, verbose=TRUE, ...)
}
\arguments{
  \item{x}{ A data frame containing one column for the event/right-censored
	times, one column indicating which times were observed (i.e. \code{time2}
	in \code{Surv}), and columns for covariates }
  \item{colTime}{ The name or number indicating which column of \code{x} is the
	event/right-censored time. }
  \item{colDelta}{ The name or number indicating which column of \code{x}
	identifies whether \code{x[,colTime]} is an observed time. }
  \item{colCov}{ A vector of the column names or numbers of \code{x} designating
	covariates to be included in the model. }
  \item{tdCov}{ The column name or number of \code{x} indicating the covariate
	to be made time-dependent. }
  \item{transformation}{ A function for transforming the time variable to comp }
  \item{outputModel}{ Whether or not the \code{coxph} model should be output. }
  \item{outputDF}{ Whether the constructed time-dependent data frame should be
	output. }
  \item{verbose}{ If \code{TRUE}, the function provides progress reports as it
	proceeds. }
  \item{...}{ Arguments to be passed to the \code{coxph} function. }
}
\details{
}
\value{
A list:
%%  \item{ }{}
}
\references{

}
\author{
David M Diez
}
\seealso{
%\code{\link{survival:coxph}}
}
\examples{


}
\keyword{ Cox proportional hazards model }
\keyword{ Cox PH model }
\keyword{ Time dependent covariates }
\keyword{ Time dependent Cox PH model }
\keyword{ Survival analysis }
\keyword{ Covariates }
