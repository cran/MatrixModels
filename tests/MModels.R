### So I see things in old saved  "R CMD check .." directories
require("Matrix")
require("MatrixModels")
sessionInfo()
packageDescription("Matrix")

## From: Torsten Hothorn <Torsten.Hothorn@uzh.ch>
## Subject: bug in model.Matrix with contrasts.arg set
## Date: Mon, 1 Mar 2021 09:34:49 +0100

## MM added  stopifnot()  and  'sparse = TRUE'

### ordered factor
d <- data.frame(x = gl(3, 3, ordered = TRUE)) # 'ordered' ==> default contrasts "contr.poly"

### default contrast works
(x <- model.matrix(~ x, data = d))
(X <- model.Matrix(~ x, data = d))
stopifnot(identical(dim(x), dim(X)), all(X == x))

### other contrasts fail -- MM now fixed
ctr <- list(x = contr.treatment)
(x <- model.matrix(~ x, data = d, contrasts.arg = ctr)) # of course fine
(X <- model.Matrix(~ x, data = d, contrasts.arg = ctr))
## gave   Error in sprintf(gettext(fmt, domain = domain), ...) : too few arguments
stopifnot(identical(dim(x), dim(X)), all(X == x))
##
(x.<- model.matrix(~ x, data = d, contrasts.arg = attr(x, "contrasts")))
(X <- model.Matrix(~ x, data = d, contrasts.arg = attr(x, "contrasts")))
(Xs<- model.Matrix(~ x, data = d, contrasts.arg = attr(x, "contrasts"), sparse=TRUE))
## gave   Error in sprintf(gettext(fmt, domain = domain), ...) : too few arguments
stopifnot(identical(dim(x), dim(X )), all(X == x),
          identical(dim(x), dim(Xs)), all(Xs== x),
          identical(x, x.))


ctr <- list(x = contr.sum)
(x <- model.matrix(~ x, data = d, contrasts.arg = ctr))
(X <- model.Matrix(~ x, data = d, contrasts.arg = ctr))
stopifnot(identical(dim(x), dim(X)), all(X == x))
## gave   Error in sprintf(gettext(fmt, domain = domain), ...) : too few arguments
(X <- model.Matrix(~ x, data = d, contrasts.arg = attr(x, "contrasts")))
## gave   Error in sprintf(gettext(fmt, domain = domain), ...) : too few arguments
stopifnot(identical(dim(x), dim(X)), all(X == x))


