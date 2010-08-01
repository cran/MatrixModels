\name{glpModel-class}
\title{Class "glpModel" of General Linear Prediction Models}
\Rdversion{1.1}
\docType{class}
\alias{glpModel-class}
\alias{coef,glpModel-method}
\alias{fitted,glpModel-method}
\alias{residuals,glpModel-method}
\description{
  The class \code{"glpModel"} conceptually contains a very large class
  of \emph{\dQuote{General Linear Prediction Models}}.

  Its \code{resp} slot (of class \code{"\linkS4class{respModule}"}) may
  model linear, non-linear, generalized linear and non-linear
  generalized response models.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("glpModel", ...)},
  but typically rather are returned by our modeling functions, e.g., the
  (experimental, hence currently hidden) \code{glm4()}.
}
\section{Slots}{
  \describe{
    \item{\code{resp}:}{a \code{"\linkS4class{respModule}"} object.}
    \item{\code{pred}:}{a \code{"\linkS4class{predModule}"} object.}
  }
}
\section{Extends}{
  Class \code{"\linkS4class{Model}"}, directly.%%-- FIXME move that stats4
}
\section{Methods}{
  \describe{
    \item{coef}{\code{signature(object = "glpModel")}: extract the
      coefficient vector \eqn{\beta} from the object.}
    \item{fitted}{\code{signature(object = "glpModel")}: fitted values;
      there may be several types, corresponding to the residuals, see there
      (below).}
    \item{residuals}{\code{signature(object = "glpModel")}: residuals,
      depending on the type of the model, there are several types of
      residuals and correspondingly residuals,
      see \code{\link[stats:glm.summaries]{residuals.glm}} from the
      \pkg{stats} package.}
  }
}
%\author{Douglas Bates}
\seealso{
  \code{\link{glm4}()} returns fitted \code{glpModel} objects.

  The constituents of this class are
  \code{\linkS4class{respModule}} and
  \code{\linkS4class{predModule}}, both of which have several sub
  classes.
}
\examples{
showClass("glpModel")

## Use   example(glm4)  or see  help(glm4) for many more examples.
}
\keyword{classes}
