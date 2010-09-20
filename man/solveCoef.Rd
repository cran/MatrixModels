\name{solveCoef}
\title{Solve for the Coefficients or Coefficient Increment}
\alias{solveCoef}
\alias{solveCoef-methods}
\alias{solveCoef,dPredModule-method}
\alias{solveCoef,sPredModule-method}
\usage{
solveCoef(predM, \dots)
}
\arguments{
  \item{predM}{prediction module, i.e. from class
    \code{\linkS4class{predModule}}.}
  \item{\dots}{potentially further arguments used in methods; not used
    currently.}
}
\description{
  The squared length of the intermediate solution is attached as an
  attribute of the returned value.
}
\value{coefficient vector or increment of coef.~vector.}
% \details{
% }
% \references{
% }
% \seealso{
% }
\section{Methods}{
  \describe{
    \item{\code{signature(predM = "dPredModule")}}{ .. }
    \item{\code{signature(predM = "sPredModule")}}{ .. }
  }
}
\examples{
## TODO
}
\keyword{regression}
\keyword{methods}
