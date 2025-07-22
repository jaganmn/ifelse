library(ifelse)
if (interactive()) options(error = recover)


## Arguments are all evaluated
x <- tryCatch(ifelse (TRUE, 1L, stop("no")), error = conditionMessage)
y <- tryCatch(ifelse1(TRUE, 1L, stop("no")), error = conditionMessage)
stopifnot(identical(x, 1L), identical(y, "no"))


## The type and class of the return value do not depend on 'test',
## are the type and class of c(yes[0], no[0], na[0])
.new <- function (x) NULL
.methods <- list("[" = function (x, i, j, ..., drop = FALSE) NULL,
                 "[<-" = function (x, i, j, ..., value) NULL,
                 "c" = function (...) NULL)
for (.class in c("A", "B")) {
    body(.new) <-
        substitute(`class<-`(x, .CLASS),
                   list(.CLASS = .class))
    assign(paste0(".", .class), .new)
    for (.generic in names(.methods)) {
        body(.methods[[.generic]]) <-
            substitute(`class<-`(NextMethod(.GENERIC), .CLASS),
                       list(.GENERIC = .generic, .CLASS = .class))
        .S3method(.generic, .class, .methods[[.generic]])
    }
}
.A
getS3method("["  , "A")
getS3method("[<-", "A")
getS3method("c"  , "A")
stopifnot(identical(ifelse (FALSE, 1L, 0), 0 ),
          identical(ifelse ( TRUE, 1L, 0), 1L),
          identical(ifelse1(FALSE, 1L, 0), 0 ),
          identical(ifelse1( TRUE, 1L, 0), 1 ),
          identical(ifelse (.A(FALSE), .B(1L), 0), .A(0 )),
          identical(ifelse (.A( TRUE), .B(1L), 0), .A(1L)),
          identical(ifelse1(.A(FALSE), .B(1L), 0), .B(0 )),
          identical(ifelse1(.A( TRUE), .B(1L), 0), .B(1 )),
          identical(ifelse (.A(FALSE), 1L, .B(0)), .A(0 )),
          identical(ifelse (.A( TRUE), 1L, .B(0)), .A(1L)),
          identical(ifelse1(.A(FALSE), 1L, .B(0)),    0  ),
          identical(ifelse1(.A( TRUE), 1L, .B(0)),    1  ))


## As above but with classes from 'base', some with additional
## attributes to be reconciled ...
identicalPOSIXlt <- function (x, y, ...) {
    if (getRversion() >= "4.3.0" &&
        inherits(x, "POSIXlt") && inherits(y, "POSIXlt"))
        identical(balancePOSIXlt(x), balancePOSIXlt(y), ...)
    else identical(x, y, ...)
}
identicalIfElse <- function (yes, no = yes)
    identicalPOSIXlt(ifelse1(yes == yes[1L], yes, no), c(yes, no[0L])) &&
    identicalPOSIXlt(ifelse1( no ==  no[1L], no, yes), c(no, yes[0L]))
L <-
list(D   <- as.Date(seq(-1, 1, 0.5)),
     Pc0 <- as.POSIXct(D, tz = "UTC"),
     PcT <- as.POSIXct(D, tz = "America/Toronto"), # platform dependent
     Pl0 <- as.POSIXlt(Pc0),
     PlT <- as.POSIXlt(PcT),
     Pds <- difftime(Pc0 + -2:2, Pc0, units = "secs"),
     Pdh <- difftime(Pc0 + -2:2, Pc0, units = "hours"),
     fAa  <- factor(c("A", "a", NA)),
     fBb  <- factor(c("B", "b", NA)))
stopifnot(all(vapply(L, identicalIfElse, FALSE)),
          ## reconciliation of attributes is consistent with:
          ## c.POSIXct
          identicalIfElse(Pc0, PcT),
          ## c.POSIXlt
          if (getRversion() >= "4.6.0" &&
              as.integer(R.version[["svn rev"]]) >= 88441L)
          identicalIfElse(Pl0, PlT)
          else
          TRUE,
          ## c.difftime
          identicalIfElse(Pds, Pdh),
          ## c.factor (special as levels are *merged*)
          identical(ifelse1(fAa == fAa[1L], fAa, fBb),
                    c(fAa[1L], fBb[2L], as.factor(NA_integer_))),
          identical(ifelse1(fBb == fBb[1L], fBb, fAa),
                    c(fBb[1L], fAa[2L], as.factor(NA_integer_))))


## Now with time series objects, for which methods for '[', 'c'
## (correctly) do *not* keep the class ...
xt <- ts(-2:2)
stopifnot(identical(          ifelse (xt == 0, xt, xt) ,           xt ),
          ## ^because the return value is based on 'test'
          identical(          ifelse1(xt == 0, xt, xt) , as.vector(xt)),
          ## ^because the return value is based on c(yes[0], no[0])
          identical(`[<-`(xt, ifelse1(xt == 0, xt, xt)),           xt )
          ## ^get "old style" behaviour explicitly by subassigning
          ##  the return value to an object of the desired class
          )


## Attributes of 'test' are discarded, with the exceptions of
## 'names', 'dim', 'dimnames'
tt <- c(FALSE, TRUE)
vv <- c(0, 1)
attributes(tt) <- aa <-
    list(names = c("a", "b"), other = 0)
stopifnot(identical(ifelse (tt, 1, 0), `attributes<-`(vv, aa)),
          identical(ifelse1(tt, 1, 0), `attributes<-`(vv, aa[1L])))
attributes(tt) <- aa <-
    c(list(dim = c(2L, 1L), dimnames = list(c("Aa", "Ab"), "Ba")), aa)
stopifnot(identical(ifelse (tt, 1, 0), `attributes<-`(vv, aa)),
          identical(ifelse1(tt, 1, 0), `attributes<-`(vv, aa[1L:3L])))


## Replacement of NA is supported
tt <- rep(c(FALSE, TRUE, NA), each = 2L)
vv <- seq_along(tt); vv. <- replace(vv, is.na(tt), NA)
stopifnot(identical(ifelse (tt, 3:4, 1:2), vv.),
          identical(ifelse1(tt, 3:4, 1:2), vv.),
          identical(ifelse1(tt, 3:4, 1:2, 5:6), vv))


## is.factor(test) is handled correctly
tt <- factor(c(FALSE, TRUE))
stopifnot(inherits(tryCatch(ifelse(tt, 1, 0), error = identity), "error"),
          identical(ifelse1(tt, 1, 0), ifelse1(as.logical(tt), 1, 0)))


## S4 objects of type "S4" are handled generically
if (requireNamespace("Matrix", quietly = TRUE)) withAutoprint({

( t4 <- new("lgeMatrix",
            Dim = c(2L, 2L),
            Dimnames = list(c("Aa", "Ab"), c("Ba", "Bb")),
            x = c(FALSE, TRUE, TRUE, TRUE)) )
( v4 <- new("dsparseVector", length = 4L, i = c(3L, 4L), x = c(3, 4)) )
( m4 <- new("dgRMatrix", Dim = c(1L, 4L), p = c(0L, 1L), j = 0L, x = 1) )

stopifnot(vapply(list(t4, v4, m4), typeof, "") == "S4",
          ## Return value is based on 'test'
          ## (lgeMatrix) *after* coercion to logical:
          identical(ifelse (t4, v4, v4), print(as.vector(v4))),
          identical(ifelse (t4, m4, m4), print(as.vector(m4))),
          ## Return value is based on c(yes[0], no[0])
          ## (dsparseVector or traditional double vector):
          identical(ifelse1(t4, v4, v4),
                    print(`dim<-`(          v4 , dim(test)))),
          identical(ifelse1(t4, m4, m4),
                    print(`dim<-`(as.vector(m4), dim(test)))))

})


## *---------------------------------------------------------------*
## |  Battery tests including comparison of ifelse1, .ifelse1 ...  |
## *---------------------------------------------------------------*

## Exclude raw vectors for now as '[<-' is minimally implemented.

arg.types <- c(if (FALSE) "raw", "logical", "integer", "double",
               if (getRversion() >= "4.6.0" &&
                   as.integer(R.version[["svn rev"]]) >= 88444L) "complex",
               "character", "list", "expression")
arg.lengths <- 0L:7L
arg.values <- c(0L:255L, NA)
set.seed(509693732L)
for (i in seq_len(128L)) {
    at <- sample(arg.types  , 4L, TRUE)
    al <- sample(arg.lengths, 4L, TRUE)
    if (at[1L] == "expression")
        at[1L] <- "list" # as.logical(<expression>) is unimplemented
    test <-           sample(c(FALSE, TRUE, NA), al[1L], TRUE)
    yes  <- as.vector(sample(        arg.values, al[2L], TRUE), at[2L])
    no   <- as.vector(sample(        arg.values, al[3L], TRUE), at[3L])
    na   <- as.vector(sample(        arg.values, al[4L], TRUE), at[4L])
    class(yes) <- "zzz" # so that ifelse1 does not use .Call
    stopifnot(identical( ifelse1(test, yes, no, strict = FALSE),
                        .ifelse1(test, yes, no)),
              identical( ifelse1(test, yes, no, na, strict = FALSE),
                        .ifelse1(test, yes, no, na)))
}
