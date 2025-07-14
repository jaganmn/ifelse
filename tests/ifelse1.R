library(ifelse)
if (interactive()) options(error = recover)


## Arguments are all evaluated
x <- tryCatch(ifelse (TRUE, 1L, stop("no")), error = conditionMessage)
y <- tryCatch(ifelse1(TRUE, 1L, stop("no")), error = conditionMessage)
stopifnot(identical(x, 1L), identical(y, "no"))


## The type and class of the return value do not depend on 'test',
## are the type and class of c(yes, no, na)
.A <- function(x) `class<-`(x, "A")
stopifnot(identical(ifelse (FALSE, 1L, 0), 0 ),
          identical(ifelse ( TRUE, 1L, 0), 1L),
          identical(ifelse1(FALSE, 1L, 0), 0 ),
          identical(ifelse1( TRUE, 1L, 0), 1 ),
          identical(ifelse (.A(FALSE), .Date(1L), 0),    .A(0 )),
          identical(ifelse (.A( TRUE), .Date(1L), 0),    .A(1L)),
          identical(ifelse1(.A(FALSE), .Date(1L), 0), .Date(0 )),
          identical(ifelse1(.A( TRUE), .Date(1L), 0), .Date(1 )),
          identical(ifelse (.A(FALSE), 1L, .Date(0)),    .A(0 )),
          identical(ifelse (.A( TRUE), 1L, .Date(0)),    .A(1L)),
          identical(ifelse1(.A(FALSE), 1L, .Date(0)),       0  ),
          identical(ifelse1(.A( TRUE), 1L, .Date(0)),       1  ))


## Attributes of 'test' are discarded, with the exceptions of
## 'names', 'dim', 'dimnames'
tt <- c(FALSE, TRUE)
vv <- c(0, 1)
attributes(tt) <- aa <-
    list(names = c("a", "b"), other = 0)
stopifnot(identical(ifelse (tt, 1, 0), `attributes<-`(vv, aa)),
          identical(ifelse1(tt, 1, 0), `attributes<-`(vv, aa[1L])))
attributes(tt) <- aa <-
    c(aa, list(dim = c(2L, 1L), dimnames = list(c("Aa", "Ab"), "Ba")))
stopifnot(identical(ifelse (tt, 1, 0), `attributes<-`(vv, aa)),
          identical(ifelse1(tt, 1, 0), `attributes<-`(vv, aa[3L:4L])))


## Replacement of NA is supported
tt <- c(FALSE, TRUE, NA)
vv <- c(0, 1, NA)
stopifnot(identical(ifelse (tt, 1, 0), vv),
          identical(ifelse1(tt, 1, 0), vv),
          identical(ifelse1(tt, 1, 0, 0), replace(vv, is.na(tt), 0)))


## is.factor(test) is handled correctly
tt <- factor(c(FALSE, TRUE))
stopifnot(inherits(tryCatch(ifelse(tt, 1, 0), error = identity), "error"),
          identical(ifelse1(tt, 1, 0), ifelse1(as.logical(tt), 1, 0)))


## S4 objects of type "S4" are handled generically (!)
if (requireNamespace("Matrix", quietly = TRUE)) withAutoprint({

( t4 <- new("lgeMatrix",
            Dim = c(2L, 2L),
            Dimnames = list(c("Aa", "Ab"), c("Ba", "Bb")),
            x = c(FALSE, TRUE, NA, NA)) )
( y4 <- new("dsparseVector", length = 4L, i = c(3L, 4L), x = c(3, 4)) )
( n4 <- new("dgRMatrix", Dim = c(1L, 4L), p = c(0L, 1L), j = 0L, x = 1) )
( vv <- array(c(1, 0, NA, NA), dim = dim(t4), dimnames = dimnames(t4)) )

stopifnot(vapply(list(t4, y4, n4), typeof, "") == "S4",
          identical(ifelse(t4, y4, n4), as.vector(vv)),
          methods::is(ifelse1(t4, y4, n4), "dMatrix"),
          identical(methods::as(ifelse1(t4, y4, n4), "matrix"), vv))

}) # using S4 classes and methods from package 'Matrix'
