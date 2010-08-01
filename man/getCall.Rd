\name{getCall}
\alias{getCall}
\alias{getCall}
%\alias{getCall,Model-method} is in ./Model-class.Rd
\title{Get 'call' Component of an R Object}
\description{
  Get the \code{\link{call}} component (or slot) of an \R{} object.
  This is a generic function, to be used instead of \code{x$call} or
  \code{x@call}.
}
\usage{
getCall(x)
}
\arguments{
  \item{x}{an \R object, typically a fitted model, possibly inheriting
    from class \code{\linkS4class{Model}}.}
}
\value{
  an object of class \code{"call"}, see \code{\link{call}}.
}
\seealso{
  \code{\link{call}};  the \code{\linkS4class{Model}} class has a
  \code{getCall} method (which is inherited to all its derived classes).
}
% \examples{
% }
\keyword{models}
