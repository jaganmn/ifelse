## Return position of object of most general class (0 <=> ill-defined).
## TODO: C translation
posCommonClass <-
function (args) {
    if (any(obj <- vapply(args, is.object, FALSE))) {
        if (!all(obj) || (any(obj4 <- vapply(args, isS4, FALSE)) && !all(obj4)))
            return(0L)
        pos <- 1L
        x <- args[[1L]]
        cx <- class(x)
        if (length(args) >= 2L)
            for (i in 2L:length(args)) {
                y <- args[[i]]
                if (isa(y, cx))
                    next
                else if (isa(x, cy <- class(y))) {
                    pos <- i
                    x <- y
                    cx <- cy
                }
                else return(0L)
            }
        if (identical(class(tryCatch(x[0L], error = as.null)), cx))
            pos
        else 0L
    } else {
        args0 <- tryCatch(unlist(lapply(args, `[`, 0L), FALSE, FALSE), error = as.null)
        if (is.null(args0))
            0L
        else match(typeof(args0), vapply(args, typeof, ""))
    }
}
