\name{reweightPred}
\title{Reweight Prediction Module Structure Internals}
\alias{reweightPred}
\alias{reweightPred-methods}
\alias{reweightPred,dPredModule,matrix,numeric-method}
\alias{reweightPred,sPredModule,matrix,numeric-method}
\usage{
reweightPred(predM, sqrtXwt, wtres, \dots)
}
\arguments{
  \item{predM}{a predictor module}
  \item{sqrtXwt}{the sqrtXwt matrix}
  \item{wtres}{the vector of weighted residuals}
  \item{\dots}{potentially further arguments used in methods; not used
    currently.}
}
\description{Update any internal structures associated with sqrtXwt and the
  weighted residuals.  The "V" matrix is evaluated from X using the
  sqrtXwt matrix and a Vtr vector is calculated.
}
\value{updated predM}
% \details{
% }
% \references{
% }
% \seealso{
% }
\section{Methods}{
  \describe{
    \item{\code{signature(predM = "dPredModule", sqrtXwt = "matrix",
	wtres = "numeric")}}{ .. }
    \item{\code{signature(predM = "sPredModule", sqrtXwt = "matrix",
	wtres = "numeric")}}{ .. }
  }
}
\examples{
## TODO
}
\keyword{regression}
\keyword{methods}
