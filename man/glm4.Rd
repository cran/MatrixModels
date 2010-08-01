\name{glm4}
\alias{glm4}
\title{Fitting Generalized Linear Models (using S4)}
\description{
  \code{glm4}, very similarly as standard \R's \code{\link{glm}()} is
  used to fit generalized linear models, specified by giving a symbolic
  description of the linear predictor and a description of the error
  distribution.

  It is more general, as it fits linear, generalized linear, non-linear
  and generalized nonlinear models.
}
\usage{
glm4(formula, family, data, weights, subset, na.action,
     start = NULL, etastart, mustart, offset,
     sparse = FALSE, doFit = TRUE, control = list(\dots),
     model = TRUE, x = FALSE, y = TRUE, contrasts = NULL, \dots)
}
\arguments{%% much cut & pasted from  glm.Rd :
  \item{formula}{an object of class \code{"\link{formula}"} (or one that
    can be coerced to that class): a symbolic description of the
    model to be fitted.  The details of model specification are given
    under \sQuote{Details}.}

  \item{family}{a description of the error distribution and link
    function to be used in the model. This can be a character string
    naming a family function, a family function or the result of a call
    to a family function.  (See \code{\link{family}} for details of
    family functions.)}

  \item{data}{an optional data frame, list or environment (or object
    coercible by \code{\link{as.data.frame}} to a data frame) containing
    the variables in the model.  If not found in \code{data}, the
    variables are taken from \code{environment(formula)},
    typically the environment from which \code{glm} is called.}

  \item{weights}{an optional vector of \sQuote{prior weights} to be used
    in the fitting process.  Should be \code{NULL} or a numeric vector.}

  \item{subset}{an optional vector specifying a subset of observations
    to be used in the fitting process.}

  \item{na.action}{a function which indicates what should happen
    when the data contain \code{NA}s.  The default is set by
    the \code{na.action} setting of \code{\link{options}}, and is
    \code{\link{na.fail}} if that is unset.  The \sQuote{factory-fresh}
    default is \code{\link{na.omit}}.  Another possible value is
    \code{NULL}, no action.  Value \code{\link{na.exclude}} can be useful.}

  \item{start, etastart, mustart}{
    starting values for the parameters in the linear predictor, the
    predictor itself and for the vector of means.}
  \item{offset}{this can be used to specify an \emph{a priori} known
    component to be included in the linear predictor during fitting.
    This should be \code{NULL} or a numeric vector of length equal to
    the number of cases.  One or more \code{\link{offset}} terms can be
    included in the formula instead or as well, and if more than one is
    specified their sum is used.  See \code{\link{model.offset}}.}
  \item{sparse}{logical indicating if the model matrix should be sparse
    or not.}
  \item{doFit}{logical indicating if the model should be fitted (or just
    returned unfitted).}
  \item{control}{
    a list with options on fitting; currently passed unchanged to
    (hidden) function \code{IRLS()}.}
  \item{model, x, y}{currently ignored; here for back compatibility with
    \code{\link{glm}}.}
  \item{contrasts}{currently ignored}%--- FIXME
  \item{\dots}{potentially arguments passed on to fitter functions; not
    used currently.}
}
% \details{
%   ...............
% }
\value{
  an object of class \code{\linkS4class{glpModel}}.
}
% \references{
% }
\seealso{
  \code{\link{glm}()} the standard \R function;\cr
  \code{\link{lm.fit.sparse}()} a sparse least squares fitter.

  The resulting class \code{\linkS4class{glpModel}} documentation.
}
\examples{
### All the following is very experimental -- and probably will change: -------

data(CO2, package="datasets")
## dense linear model
str(glm4(uptake ~ 0 + Type*Treatment, data=CO2, doFit = FALSE), 4)
## sparse linear model
str(glm4(uptake ~ 0 + Type*Treatment, data=CO2, doFit = FALSE,
                  sparse = TRUE), 4)

## From example(glm): -----------------

## Dobson (1990) Page 93: Randomized Controlled Trial :
str(trial <- data.frame(counts=c(18,17,15,20,10,20,25,13,12),
                        outcome=gl(3,1,9,labels=LETTERS[1:3]),
                        treatment=gl(3,3,labels=letters[1:3])))
glm.D93 <- glm(counts ~ outcome + treatment, family=poisson, data=trial)
summary(glm.D93)
c.glm <- unname(coef(glm.D93))
glmM  <- glm4(counts ~ outcome + treatment, family = poisson, data=trial)
glmM2 <- update(glmM, quick = FALSE) # slightly more accurate
glmM3 <- update(glmM, quick = FALSE, finalUpdate = TRUE)
                 # finalUpdate has no effect on 'coef'
stopifnot( identical(glmM2@pred@coef, glmM3@pred@coef),
           all.equal(glmM @pred@coef, c.glm, tol=1e-7),
           all.equal(glmM2@pred@coef, c.glm, tol=1e-12))
\dontshow{
stopifnot( ## ensure typos are *caught* :
  inherits(try(glm4(counts ~ outcome + treatment, family=poisson, data=trial,
                       fooBar = FALSE)), "try-error"),
  ## check formula(.): {environments differ - FIXME?}
  formula(glmM) == formula(glm.D93))
}

## Watch the iterations --- and use no intercept --> more sparse X
## 1) dense generalized linear model
glmM <- glm4(counts ~ 0+outcome + treatment, poisson, trial,
                      verbose = TRUE)
## 2) sparse generalized linear model
glmS <- glm4(counts ~ 0+outcome + treatment, poisson, trial,
                      verbose = TRUE, sparse = TRUE)
str(glmS, max.lev = 4)
stopifnot( all.equal(glmM@pred@coef, glmS@pred@coef),
           all.equal(glmM@pred@Vtr,  glmS@pred@Vtr) )


## A Gamma example, from McCullagh & Nelder (1989, pp. 300-2)
clotting <- data.frame(u = c(5,10,15,20,30,40,60,80,100),
                       lot1 = c(118,58,42,35,27,25,21,19,18),
                       lot2 = c(69,35,26,21,18,16,13,12,12))
str(gMN <- glm4(lot1 ~ log(u), data=clotting, family=Gamma, verbose=TRUE))
glm. <- glm(lot1 ~ log(u), data=clotting, family=Gamma)
stopifnot( all.equal(gMN@pred@coef, unname(coef(glm.)), tol=1e-7) )
}
\keyword{models}
\keyword{regression}

