ifelse1 <-
function (test, yes, no, na = NULL, maybeDotCall = FALSE)
{
    ltest <-
        if (is.logical(test) && !is.object(test))
            test
        else if (isS4(test))
            methods::as(test, "logical")
        else as.logical(test)
    if (maybeDotCall &&
        !(is.object(yes) || is.object(no) || is.object(na)))
        ans <- .Call(R_ifelse_ifelse1, ltest, yes, no, na)
    else {
        dft0 <- `names<-`(c(yes[0L], no[0L], na[0L]), NULL)
        dft1 <- dft0[1L]
        ntest <- length(ltest)
        if (ntest == 1L) {
            tmp <- if (is.na(ltest)) na else if (ltest) yes else no
            ans <- if (length(tmp)) c(dft0, `names<-`(tmp[1L], NULL)) else dft1
        } else {
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
    }
    ## Take care to dispatch methods for 'names', 'dim', 'dimnames'
    ## and the replacement functions:
    dim(ans) <- dim(test)
    dimnames(ans) <- dimnames(test)
    names(ans) <- names(test)
    ans
}
