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
        ntest <- length(ltest)
        if (ntest == 1L) {
            ans <- `names<-`(
                if (is.na(ltest))
                    c(yes[0L], no[0L], na[1L])[1L]
                else if (ltest)
                    c(yes[1L], no[0L], na[0L])
                else c(yes[0L], no[1L], na[0L]), NULL)
        } else {
            ans <- rep(`names<-`(c(yes[0L], no[0L], na[0L]), NULL),
                       length.out = ntest)
            if (length(j <- which(ltest)))
                ans[j] <- if ((n <- length(yes)) == 1L) yes
                          else if (n >= ntest) yes[j]
                          else rep(yes, length.out = ntest)[j]
            if (length(j <- which(!ltest)))
                ans[j] <- if ((n <- length(no)) == 1L) no
                          else if (n >= ntest) no[j]
                          else rep(no, length.out = ntest)[j]
            if (!is.null(na) &&
                length(j <- which(is.na(ltest))))
                ans[j] <- if ((n <- length(na)) == 1L) na
                          else if (n >= ntest) na[j]
                          else rep(na, length.out = ntest)[j]
        }
    }
    ## Take care to dispatch methods for 'names', 'dim', 'dimnames'
    ## and the replacement functions:
    if (is.null(d <- dim(test)))
        names(ans) <- names(test)
    else {
        dim(ans) <- d
        dimnames(ans) <- dimnames(test)
    }
    ans
}
