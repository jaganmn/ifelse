.ifelse1 <-
function (test, yes, no, na = NULL)
    .Call(R_ifelse_ifelse1, test, yes, no, na)

ifelse1 <-
function (test, yes, no, na = NULL, strict = FALSE)
{
    ltest <-
        if (is.logical(test) && !is.object(test))
            test
        else if (isS4(test))
            methods::as(test, "logical")
        else as.logical(test)
    ntest <- length(ltest)
    if (is.na(strict) || strict) {
        n <- c(length(yes), length(no), if (is.null(na)) 1L else length(na))
        if (length(k <- which(n != 1L & n != ntest)))
            (if (is.na(strict)) warning else stop)(
                gettextf("length(%s) [%.0f] not equal to 1 or length(%s) [%.0f]",
                         c("yes", "no", "na")[k[1L]], n[k[1L]],
                         "test", ntest),
                domain = NA)
    }
    if (!(is.object(yes) || is.object(no) || is.object(na)))
        return(.Call(R_ifelse_ifelse1, ltest, yes, no, na))
    ## At least one of 'yes', 'no', 'na' has a class attribute, and we
    ## expect the following:
    ##
    ## [1] That 'c' works for class(yes).
    ## [2] That '[', 'length' work for class(yes), class(no), class(na).
    ## [3] That '[<-' works for class(c(yes[0L], no[0L], na[0L])).
    ## [4] That x[0L][1L] is the canonical "missing value" for class(x).
    ##
    ## A known limitation is that '[' and 'c' may not preserve the class
    ## of time series objects.  '[<-' with missing subscript provides a
    ## simple work-around:
    ##
    ## > x <- ts(-1:1); y <- ifelse1(x == 0, x, x)
    ## > stopifnot(identical(x, `[<-`(x, y)), identical(y, as.vector(x)))
    ##
    dft <- `names<-`(c(yes[0L], no[0L], na[0L]), NULL)
    if (ntest == 1L) {
        tmp <- if (is.na(ltest)) na else if (ltest) yes else no
        ans <- if (length(tmp)) c(dft, `names<-`(tmp[1L], NULL)) else dft[1L]
    } else {
        ans <- dft[seq_len(ntest)]
        ## NOT: `length<-`(dft, ntest)       # as needs 'length<-'
        ## NOT: rep(dft, length.out = ntest) # as needs 'rep'
        if ((n <- length(yes)) && length(j <- which(ltest)))
            ans[j] <- if (n == 1L) yes
                      else if (n >= ntest) yes[j]
                      else yes[1L + (j - 1L) %% n]
        if ((n <- length(no)) && length(j <- which(!ltest)))
            ans[j] <- if (n == 1L) no
                      else if (n >= ntest) no[j]
                      else no[1L + (j - 1L) %% n]
        if ((n <- length(na)) && length(j <- which(is.na(ltest))))
            ans[j] <- if (n == 1L) na
                      else if (n >= ntest) na[j]
                      else na[1L + (j - 1L) %% n]
    }
    ## We take care to dispatch methods for 'names', 'dim', 'dimnames'
    ## and the replacement functions and *not* rely on attributes which
    ## need not exist.
    ##
    ## > loadNamespace("Matrix"); names(attributes(new("lgeMatrix")))
    ##
    ## We must work around missing dim<-.POSIXlt, dimnames<-.POSIXlt.
    ## The internal default method does not do the right thing!
    ##
    ## > x <- as.POSIXlt(.POSIXct(0, "UTC"))
    ## > `dim<-`(x, length(        x ))
    ## > `dim<-`(x, length(unclass(x)))
    ##
    ## Indeed, we expect implicitly that the replacement functions work
    ## for class(ans).
    ##
    if (!is.null(a <- dim(test)) && !inherits(ans, "POSIXlt")) {
        dim(ans) <- a
        if (!is.null(a <- dimnames(test)))
            dimnames(ans) <- a
    }
    if (!is.null(a <- names(test)))
        names(ans) <- a
    ans
}
