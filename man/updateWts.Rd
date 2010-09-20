\name{updateWts}
\title{Update the Residual and X Weights - Generic and Methods}
\alias{updateWts}
\alias{updateWts-methods}
\alias{updateWts,glmRespMod-method}
\alias{updateWts,respModule-method}
\usage{
updateWts(respM, \dots)
}
\arguments{
  \item{respM}{a response module, see the
    \code{\linkS4class{respModule}} class.}
  \item{\dots}{potentially further arguments used in methods; not used
    currently.}
}
\description{Update the residual weights \code{sqrtrwt} and
  \eqn{X} weights \code{sqrtXwt}.
}
\section{Methods}{
  \describe{
    \item{\code{signature(respM = "glmRespMod")}}{ .. }
    \item{\code{signature(respM = "respModule")}}{ .. }
  }
}
% \details{
% }
\value{updated response module.
}
% \references{
% }
% \seealso{
% }
\examples{
## TODO
}
\keyword{regression}
\keyword{methods}
