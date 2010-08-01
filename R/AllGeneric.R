##' <description>
##' Updates the mean vector mu given the linear predictor
##' gamma. Evaluate the residuals and the weighted sum of squared
##' residuals.
##' <details>
##' Note that the offset is added to the linear predictor before
##' calculating mu.
##'
##' The sqrtXwt matrix can be updated but the sqrtrwt should not be in
##' that the weighted sum of squared residuals should be calculated
##' relative to fixed weights.  Reweighting is done in a separate call.
##' @title Update the fitted mean response
##' @param respM a response module
##' @param gamma the value of the linear predictor before adding the offset
##' @param ...
##' @return updated respM
setGeneric("updateMu", function(respM, gamma, ...)
           standardGeneric("updateMu"))

##' <description>
##' Update the weights, sqrtrwt and sqrtXwt
##' <details>
##' @title Update the residual and X weights
##' @param respM a response module
##' @param ...
##' @return updated response module
setGeneric("updateWts", function(respM, ...)
           standardGeneric("updateWts"))

if (FALSE) {                            # don't need this generic in R
##' <description>
##' Set new values of the coefficients.  Can be called with a single
##' vector argument and with a pair of vectors, representing a base and
##' an increment, plus a step factor.
##' <details>
##' @title set new values of the coefficients
##' @param predM a predictor module
##' @param base coefficient base value
##' @param incr increment
##' @param step step factor, defaults to 0 in which case incr is ignored
##' @param ...
##' @return predM
setGeneric("setCoef", function(predM, base, incr, step = 0, ...) standardGeneric("setCoef"))
}

##' <description>
##' Update any internal structures associated with sqrtXwt and the
##' weighted residuals.  The "V" matrix is evaluated from X using the
##' sqrtXwt matrix and a Vtr vector is calculated.
##' <details>
##' @title
##' @param predM a predictor module
##' @param sqrtXwt the sqrtXwt matrix
##' @param wtres the vector of weighted residuals
##' @param ...
##' @return updated predM
setGeneric("reweight", function(predM, sqrtXwt, wtres, ...)
           standardGeneric("reweight"))

if (FALSE) {                       # don't nee this generic in R
##' <description>
##' Return the gamma vector
##' <details>
##' @title
##' @param predM a predictor module
##' @param ...
##' @return X %*% coef
setGeneric("gammaInc", function(predM, ...)
           standardGeneric("gammaInc"))
}

##' <description>
##' Solve for the coefficients, usually in the form of
##' coef <- solve(predM@fac, predM@Vtr, system = "A")
##' <details>
##' The squared length of the intermediate solution is attached as an
##' attribute of the returned value.
##' @title solve for the coefficients or coefficient increment
##' @param predM
##' @param ...
##' @return coefficient vector or increment
setGeneric("solveCoef", function(predM, ...)
           standardGeneric("solveCoef"))

##' <description>
##'
##' <details>
##' @title simple accessor to get the "call" component from a fitted model
##' @param x a (fitted) model
##' @return a "call" object
setGeneric("getCall", function(x) standardGeneric("getCall"),
	   valueClass = "call")
