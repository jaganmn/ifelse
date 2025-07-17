.ifelse1 <-
function (test, yes, no, na = NULL)
    .Call(R_ifelse_ifelse1, test, yes, no, na)

ifelse1 <-
function (test, yes, no, na = NULL, strict = NA)
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
    ## Get length-0 and length-1 *unnamed* vectors of the final type
    ## and class.  For vector types, the latter is raw(1L) or NA or
    ## NULL.
    dft0 <- `names<-`(c(yes[0L], no[0L], na[0L]), NULL)
    dft1 <- dft0[1L]
    if (ntest == 1L) {
        ## Be fast here:
        tmp <- if (is.na(ltest)) na else if (ltest) yes else no
        ans <- if (length(tmp)) c(dft0, `names<-`(tmp[1L], NULL)) else dft1
    } else {
        ## Avoid allocations by 'rep' wherever possible:
        ans <- rep(dft1, length.out = ntest)
        if ((n <- length(yes)) && length(j <- which(ltest)))
            ans[j] <- if (n == 1L) yes
                      else if (n >= ntest) yes[j]
                      else rep(yes, length.out = ntest)[j]
        if ((n <- length(no)) && length(j <- which(!ltest)))
            ans[j] <- if (n == 1L) no
                      else if (n >= ntest) no[j]
                      else rep(no, length.out = ntest)[j]
        if ((n <- length(na)) && length(j <- which(is.na(ltest))))
            ans[j] <- if (n == 1L) na
                      else if (n >= ntest) na[j]
                      else rep(na, length.out = ntest)[j]
    }
    ## Take care to dispatch methods for 'names', 'dim', 'dimnames' and
    ## the replacement functions and *not* rely on attributes which need
    ## not exist.
    ## > loadNamespace("Matrix"); names(attributes(new("lgeMatrix")))
    if (!is.null(a <- dim(test))) {
        dim(ans) <- a
        if (!is.null(a <- dimnames(test)))
            dimnames(ans) <- a
    }
    if (!is.null(a <- names(test)))
        names(ans) <- a
    ans
}
