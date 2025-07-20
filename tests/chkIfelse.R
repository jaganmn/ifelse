## Adapted from MM's GH gist:
##   https://gist.github.com/mmaechler/9cfc3219c4b89649313bfe6853d87894
## To see the changes:
##   $ diff -u ./chkIfelse.R.orig ./chkIfelse.R
##
##
## <MM>
##
##


##' warnifnot(): a "only-warning" version of stopifnot():
##'   {Yes, learn how to use do.call(substitute, ...) in a powerful manner !!}
warnifnot <- stopifnot
body(warnifnot) <- do.call(substitute, list(body(stopifnot),
                                            list(stop = quote(warning))))
## (now, this was really cute ....)


##' @title Simplistic Checking of Different  ifelse()  Implementations
##' @param FUN a function \dQuote{similar but "better" than} \code{ifelse()}
##' @param nFact positive integer specifying the number factor() checks
##' @param noRmpfr logical specifying to skip \pkg{Rmpfr} examples
chkIfelse <- function(FUN, nFact = 500, NULLerror = TRUE, noRmpfr = FALSE) {
    FUN <- match.fun(FUN)
    if(NULLerror) {
        op <- options(error = NULL); on.exit( options(op) )
    }
    rTF <- function(n, pr.T)
        sample(c(TRUE,FALSE), n, replace=TRUE, prob = c(pr.T, 1-pr.T))
    Try <- function(expr) tryCatch(expr, error = identity)
    chkArith <- function(T., yes, no, trafo = identity, class. = class(yes), ...) {
        r <- Try(FUN(T., yes, no))
        warnifnot(
            ii <- inherits(r, class.),
            if(ii) all.equal(trafo(r),
                             T.*trafo(yes) + (!T.)*trafo(no), ...) else TRUE)
        invisible(r)
    }
    if((has.4th <- length(formals(FUN)) >= 4))
	if(names(formals(FUN))[[4]] %in% "missing") { # we use 'NA.' below
	    body(FUN) <- do.call(substitute,
				 list(body(FUN),
				      setNames(list(quote(NA.)), names(formals(FUN))[[4]])))
	    names(formals(FUN))[[4]] <- "NA."
	}
	else # not a "NA." / "missing" like meaning of 4th argument
	    has.4th <- FALSE

    rid <- FUN(c(TRUE, FALSE,   TRUE),   1:3, 100*(1:3)) # int / double
    rdd <- FUN(c(TRUE, FALSE,   TRUE), 0+1:3, 100*(1:3))
    rdN <- FUN(c(TRUE, FALSE,NA,TRUE), 0+1:4, 100*(1:4))
    warnifnot(all.equal(rid, rdd, tol = 0),
              all.equal(rdN, c(1,200, NA, 4), tol = 0))
    if(has.4th)
        warnifnot(identical(
            FUN(c(TRUE, FALSE,NA,TRUE), 0+ 1:4, 100*(1:4), NA. = -909),
            c(1, 200, -909, 4)))
    ##--- matrices -------------------------------
    for(i in 1:20) { ## ifelse() keeps attributes (from 'test')
        r10 <- round(10 * rnorm(10))
        m2 <- cbind(10:1, r10)
        rm2 <- FUN(m2 >= -2, m2, abs(m2))
        warnifnot(iM <- is.matrix(rm2))
        if(iM) warnifnot(identical(rm2[,2], FUN(r10 >= -2 , r10, abs(r10))))
    }
    r10 <- c(-2, 27, -7, 4, -3, 2, -1, -4, -3, -8)
    ##--- Date-Time objects: -----------------------
    rD <- FUN(c(TRUE, FALSE, TRUE), Sys.Date(), as.Date("2016-11-11")) # Date
    warnifnot(inherits(rD, "Date"))
    ch03 <- paste0("2003-",rep(1:4, 4:1), "-", sample(1:28, 10, replace=TRUE))
    x03ct <- as.POSIXct(ch03)
    x03D  <- as.Date(ch03)
    for(i in 1:20)
        warnifnot(identical(x03D,  FUN(rTF(10, pr.=0.4), x03D,  x03D )),
                  identical(x03ct, FUN(rTF(10, pr.=0.7), x03ct, x03ct)))
    ct <- Sys.time(); lt <- as.POSIXlt(ct)
    ifct <- FUN(c(TRUE, FALSE, NA, TRUE), ct, ct-100)# POSIXct
    iflt <- FUN(c(TRUE, FALSE, NA, TRUE), lt, lt-100)# POSIXlt/ct "mix"
    ifll <- FUN(c(TRUE, FALSE, NA, TRUE), lt, as.POSIXlt(lt-100))# POSIXlt
    warnifnot(ic <- inherits(ifct, "POSIXct"),
              il <- inherits(ifll, "POSIXlt"),
              !ic || identical(balancePOSIXlt(ifll), as.POSIXlt(ifct)),
              !il || identical(ifll, as.POSIXlt(iflt))
              )
    ## POSIX*t now with 'tzone' / TZ --- ("the horror"): ----
    tzs <- c("UTC", "EST", "EST5EDT")
    x03lt.s <- sapply(tzs, as.POSIXlt, x = x03ct, simplify=FALSE)
    x03ct.s <- lapply(x03lt.s, as.POSIXct)
    for(y in tzs)
        for(n in tzs) {
            T. <- rTF(10, pr. = 0.4)
            ##              v             v
            chkArith(T., x03lt.s[[y]], x03lt.s[[n]], trafo = as.numeric)
            chkArith(T., x03lt.s[[y]], x03ct.s[[n]], trafo = as.numeric)
            chkArith(T., x03ct.s[[y]], x03lt.s[[n]], trafo = as.numeric)
            chkArith(T., x03ct.s[[y]], x03ct.s[[n]], trafo = as.numeric)
            ##              ^             ^
        }

    ##-- "difftime" another "atomic-like" base S3 class:
    dt.h <- as.difftime(c(1,20,60), units = "hours")
    dt.m <- as.difftime(c(1,30,60), units = "mins")
    dt.s <- as.difftime(c(1,30,60), units = "secs")
    Tst <- c(TRUE, FALSE, TRUE)
    warnifnot(
        ## the easy ones: *same* units
        identical(dt.h, FUN(Tst, dt.h, dt.h)),
        identical(dt.m, FUN(Tst, dt.m, dt.m)),
	identical(dt.s, FUN(Tst, dt.s, dt.s)))
    ## the tough ones
    chkArith(Tst, dt.h, dt.m, trafo = function(.) as.double(., "hours"), tol = 1e-14)
    chkArith(Tst, dt.h, dt.s, trafo = function(.) as.double(.,  "mins"), tol = 1e-14)
    chkArith(Tst, dt.s, dt.m, trafo = function(.) as.double(.,  "secs"), tol = 1e-14)

    ## now the "factor": -----------------------------------
    f1 <- suppressWarnings(
        FUN(c(TRUE, FALSE), factor(2:3), factor(3:4)))# "works" with warning
    warnifnot(is.factor(f1), length(f1) == 2)# not much more

    ff <- gl(11,5, labels=LETTERS[1:11]); yes <- ff; no <- rev(ff)
    llev <- levels(ff)[length(levels(ff))]
    for(i in seq_len(nFact)) {
        test <- sample(c(TRUE,FALSE, NA), length(ff), TRUE)
        r  <- FUN(test, yes, no)
        if(has.4th) {
            rN <- suppressWarnings(FUN(test, yes, no, NA. = "Z"))
            warnifnot(identical(r, rN))
            rN <- FUN(test, yes, no, NA. = llev)
        } else {
            rN <- r
            rN[is.na(rN)] <- llev
        }
        tst.T <-  test & !is.na(test)
        tst.F <- !test & !is.na(test)
        warnifnot(is.factor(r), identical(levels(r), levels(ff)),
                  r[tst.T] == yes[tst.T], identical(r[tst.T], rN[tst.T]),
                  r[tst.F] ==  no[tst.F], identical(r[tst.F], rN[tst.F]),
                  all(is.na(r[is.na(test)])),
                  all(rN[is.na(test)] == llev))
    }

    if(!require("Matrix"))
        stop("Your R installation is broken: The 'Matrix' package must be available")
    if(packageVersion("Matrix") >= "1.2-8") {
        sv1 <- sparseVector(x =   1:10,  i = sample(999, 10), length=1000L)
        sv2 <- sparseVector(x = -(1:50), i = sample(300, 10), length= 300L)#-> recycling
        ssv2 <- rep(sv2, length.out = length(sv1))
        rsv1 <- FUN(sv1 != 0, sv1, sv2)
        rsv2 <- FUN(sv2 != 0, sv2, sv1)
         vv1 <- as(sv1, "vector");        vv2 <- as(sv2, "vector")
        rvv1 <- FUN(vv1 != 0, vv1, vv2); rvv2 <- FUN(vv2 != 0, vv2, vv1)
        warnifnot(is(rsv1, "sparseVector"), is(rsv2, "sparseVector"),
                  rvv1 == rsv1, rvv2 == rsv2,
                  identical(sv1, FUN(sv1 != 0, sv1, sv1)),
                  identical(sv2, FUN(sv2 != 0, sv2, sv2)),
                  identical(sv1, FUN(sv1 == 0, sv1, sv1)),
                  identical(sv2, FUN(sv2 == 0, sv2, sv2)),
                  TRUE)
        sM1 <- Matrix(sv1, 50,20)
        sM2 <- Matrix(sv2, 30,10)
        rsM1 <- FUN(sM1 != 0, sM1, sM2)
        rsM2 <- FUN(sM2 != 0, sM2, sM1)
        warnifnot(is(rsM1, "sparseMatrix"), is(rsM2, "sparseMatrix"),
                  all.equal(rsv1, rsM1, tol=0, check.class = FALSE),
                  all.equal(rsv2, rsM2, tol=0, check.class = FALSE),
                  identical(sM1, FUN(sM1 > 0, sM1, sM1)),
                  identical(sM2, FUN(sM2 > 0, sM2, sM2)))
    }
    z5 <- c(-4, -12, -1, 16, 7)
    ## and these
    if(require("Rmpfr")) {
        r1 <- FUN(c(TRUE, FALSE,TRUE,TRUE), mpfr(1:4, 64), mpfr(10*(1:4),64))
        warnifnot(inherits(r1, "mpfr"), r1 == c(1, 20,   3:4))
	if(has.4th) {
	    r2 <- FUN(c(TRUE, FALSE, NA ,TRUE), mpfr(1:4, 64), mpfr(10*(1:4),64),
		      NA. = mpfr(-999,10))
	    warnifnot(inherits(r2, "mpfr"), r2 == c(1, 20, -999, 4))
	}
        ## and some "gmp" checks ("Rmpfr" requires "gmp")
        ZZ <- as.bigz(1:7)^50
        rZZ <- FUN(rTF(7, .4), ZZ, ZZ)
        warnifnot(inherits(rZZ, "bigz"), all(ZZ == rZZ))
        ## warnifnot(identical(ZZ, )) ## <<< gmp bug (not so easy to fix ??)
    } else if(!noRmpfr)
        message("not testing 'Rmpfr' ..")

    if(require("zoo")) {
        z1 <- as.zoo(z5)
        zt <- as.zoo(ts(z5, start = 1981, freq = 12))
        zM <- suppressWarnings(zoo(cbind(10:1, rnorm(10)), x03D))
        warnifnot(identical(z1, FUN(z1 > 1, z1, z1)),
                  identical(zt, FUN(zt > 1, zt, zt)),
                  TRUE ## Fails; problem? identical(zM, FUN(zM > 1, zM, zM))
                  )
        ## TODO more?
    } else
        message("not testing 'zoo' ..")

    invisible(TRUE)
}# end{chkIfelse}


##
##
## </MM>
##
##
## Now *use* it:


options(warn = 1L, warnPartialMatchArgs = FALSE)
chkIfelse(ifelse::ifelse1)
