\name{updateMu}
\title{Update 'mu', the Fitted Mean Response}
\alias{updateMu}
\alias{updateMu-methods}
\alias{updateMu,glmRespMod,numeric-method}
\alias{updateMu,nglmRespMod,numeric-method}
\alias{updateMu,nlsRespMod,numeric-method}
\alias{updateMu,respModule,numeric-method}
\usage{
updateMu(respM, gamma, \dots)
}
\arguments{
  \item{respM}{a response module, see the
    \code{\linkS4class{respModule}} class.}
  \item{gamma}{the value of the linear predictor before adding the offset}
  \item{\dots}{potentially further arguments used in methods; not used
    currently.}
}
\description{
  Updates the mean vector \eqn{\mu} given the linear predictor
  \eqn{\gamma}.  Evaluate the residuals and the weighted sum of squared
  residuals.
}
\details{
  Note that the offset is added to the linear predictor before
  calculating mu.

  The sqrtXwt matrix can be updated but the sqrtrwt should not be in
  that the weighted sum of squared residuals should be calculated
  relative to fixed weights.  Reweighting is done in a separate call.
}
\value{updated \code{respM}}
\section{Methods}{
  \describe{
    \item{\code{signature(respM = "glmRespMod", gamma = "numeric")}}{ .. }
    \item{\code{signature(respM = "nglmRespMod", gamma = "numeric")}}{ .. }
    \item{\code{signature(respM = "nlsRespMod", gamma = "numeric")}}{ .. }
    \item{\code{signature(respM = "respModule", gamma = "numeric")}}{ .. }
  }
}
% \references{
% }
\seealso{
  The \code{\linkS4class{respModule}} class (and specific subclasses);
  \code{\link{glm4}}.
}
\examples{
## TODO
}
\keyword{regression}
\keyword{methods}

