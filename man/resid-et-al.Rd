%% FIXME:  move to stats4
\name{resid-et-al}
\title{Aliases for Model Extractors}
\alias{resid,ANY-method}
\alias{fitted.values,ANY-method}
\alias{coefficients,ANY-method}
\description{
  Aliases for model extractors; it is an old S and \R tradition to have
  aliases for these three model extractor functions:
  \describe{
    \item{\code{resid()}}{equivalent to \code{\link{residuals}()}.}
    \item{\code{fitted.values()}}{equivalent to \code{\link{fitted}()}.}
    \item{\code{coefficients()}}{equivalent to \code{\link{coef}()}.}
  }
  We provide S4 generics and methods for these.
}
\seealso{
  \code{\link{residuals}};
  \code{\link{Methods}} for general information about formal (S4) methods.
}
\section{Methods}{
    \describe{
    \item{resid}{\code{signature(object = "ANY")}: return the residuals;
      this is a rarely used \emph{alias} for \code{\link{residuals}()}.}
    \item{fitted.values}{\code{signature(object = "ANY")}: return the
      fitted values; this is a rarely used \emph{alias} for \code{\link{fitted}()}.}
    \item{coefficients}{\code{signature(object = "ANY")}: return the
      coefficients of a model; this is a rarely used \emph{alias} for
      \code{\link{coef}()}.}
  }
}
\keyword{models}
