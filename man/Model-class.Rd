%% "FIXME" --- move this to  stats4
\name{Model-class}
\title{Mother Class "Model" all S4 Models}
\Rdversion{1.1}
\docType{class}
\alias{Model-class}
\alias{formula,Model-method}
\alias{getCall,Model-method}
\alias{update,Model-method}
\description{
  Class \code{"Model"} is meant to be the mother class of all (S4) model
  classes.
  As some useful methods are already defined for \code{"Model"} objects,
  derived classes inherit those \dQuote{for free}.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
  \describe{
    \item{\code{call}:}{the \code{\link{call}} which generated the model.}
    \item{\code{fitProps}:}{a \code{\link{list}}; must be named,
      i.e., have unique \code{\link{names}}, but can be empty.

      When the main object is a \emph{fitted} model, the list will
      typically have components such as \code{iter} (non-negative
      integer) and \code{convergenece} (\code{\link{logical}} typically).
    }
  }
}
\section{Methods}{
  \describe{
    \item{formula}{\code{signature(x = "Model")}: extract the model
      formula - if there is one, or \code{\link{NULL}}.}
    \item{getCall}{\code{signature(x = "Model")}: the model generating
      \code{\link{call}}, here identical to \code{x@call}.}
    \item{update}{\code{signature(object = "Model")}: Update the model
      with a new formula, new data, \dots\dots etc.  This semantically
      equivalent (and as \R function almost identical) to the standard
      \code{\link[stats]{update}} (package \pkg{stats}).}
  }
}
\seealso{
  the \code{\link[MatrixModels:glpModel-class]{glpModel}} class in package
  \pkg{MatrixModels} which extends this class.
}
\examples{
showClass("Model")
}
\keyword{classes}
