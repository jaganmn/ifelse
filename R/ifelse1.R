ifelse1 <-
function (test, yes, no, na = NULL, maybeDotCall = FALSE)
{
    if (is.atomic(test)) {
        if (!is.logical(test))
            storage.mode(test) <- "logical"
    }
    else test <- if (isS4(test)) methods::as(test, "logical") else as.logical(test)
    if (maybeDotCall &&
        !(is.object(yes) || is.object(no) || is.object(na)))
        ans <- .Call(R_ifelse_ifelse1, test, yes, no, na)
    else {
        ntest <- length(test)
        if (ntest == 1L) {
            ans <- `names<-`(
                if (is.na(test))
                    c(yes[0L], no[0L], na[1L])[1L]
                else if (test)
                    c(yes[1L], no[0L], na[0L])
                else c(yes[0L], no[1L], na[0L]), NULL)
        } else {
            ans <- rep(`names<-`(c(yes[0L], no[0L], na[0L]), NULL),
                       length.out = ntest)
            if (length(j <- which(test)))
                ans[j] <- if ((n <- length(yes)) == 1L) yes
                          else if (n >= ntest) yes[j]
                          else rep(yes, length.out = ntest)[j]
            if (length(j <- which(!test)))
                ans[j] <- if ((n <- length(no)) == 1L) no
                          else if (n >= ntest) no[j]
                          else rep(no, length.out = ntest)[j]
            if (!is.null(na) &&
                length(j <- which(is.na(test))))
                ans[j] <- if ((n <- length(na)) == 1L) na
                          else if (n >= ntest) na[j]
                          else rep(na, length.out = ntest)[j]
        }
    }
    at <- attributes(test)
    if (!is.null(at)) {
        if (is.object(test) && !is.object(ans))
            at[["class"]] <- NULL
        if (length(at)) {
            at. <- attributes(ans)
            if (!is.null(at.))
                at[names(at.)] <- at.
            attributes(ans) <- at
        }
        if (!is.null(stats::tsp(ans)))
            class(ans) <-
                if (is.matrix(ans))
                    c("mts", "ts", "matrix", "array", oldClass(ans))
                else c("ts", oldClass(ans))
    }
    ans
}
