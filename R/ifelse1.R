ifelse1 <-
function (test, yes, no, maybeDotCall = FALSE)
{
    if (is.atomic(test)) {
        if (!is.logical(test))
            storage.mode(test) <- "logical"
    }
    else test <- if (isS4(test)) methods::as(test, "logical") else as.logical(test)
    if (maybeDotCall && !is.object(yes) && !is.object(no))
    ans <- .Call(R_ifelse_ifelse1, test, yes, no)
    else {
    nt <- length(test)
    if (nt == 1L) {
        ans <- `names<-`(
            if (is.na(test))
                c(yes[0L], no[0L])[1L]
            else if (test)
                c(yes[1L], no[0L])
            else c(yes[0L], no[1L]), NULL)
    } else {
        ans <- rep(`names<-`(c(yes[0L], no[0L]), NULL), length.out = nt)
        ny <- length(yes)
        nn <- length( no)
        jy <- which( test)
        jn <- which(!test)
        if (length(jy))
            ans[jy] <- if (ny == 1L) yes else if (ny >= nt) yes[jy] else rep(yes, length.out = nt)[jy]
        if (length(jn))
            ans[jn] <- if (nn == 1L)  no else if (nn >= nt)  no[jn] else rep( no, length.out = nt)[jn]
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
