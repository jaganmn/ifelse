Sys.setenv(OMP_THREAD_LIMIT = "1")
set.seed(1790681255L)

n <- 0x1p+24L
jn <- sample(0L:255L, n, TRUE); j1 <- jn[1L]
test <- sample(c(FALSE, TRUE, NA), n, TRUE)
class(jn) <- class(j1) <-
    "zzz" # so that ifelse1 does not use .Call

st. <-
function (ifelse, ..., r = 1024L)
{
    list(...)
    cat(sprintf("ifelse = %s\n", deparse(substitute(ifelse))))
    print(system.time(for (i in seq_len(r)) ifelse(...)))
}

st <-
function (...)
{
    d <- nargs()
    dn <- list(`Lengths:` = c("test", "yes", "no", "na")[seq_len(d)])
    print(array(lengths(list(...)), dim = d, dimnames = dn))
    if (d <= 3L)
    st.(      base ::   ifelse, ...)
    st.(    ifelse ::  ifelse1, ...)
    st.(    ifelse :: .ifelse1, ...)
    if (requireNamespace("data.table"))
    st.(data.table ::  fifelse, ...)
    if (requireNamespace("dplyr"))
    st.(     dplyr ::  if_else, ...)
    if (requireNamespace("hutils"))
    st.(    hutils ::  if_else, ...)
    if (FALSE && requireNamespace("kit"))
    st.(       kit ::      iif, ...)
    ##
    ## MJ: not eager to diagnose this error message right now ...
    ##
    ## OMP: Error #15:
    ## Initializing libomp.a, but found libomp.a already initialized.
    ##
    cat("\n\n")
    invisible(NULL)
}

st(test, jn, jn)
st(test, j1, j1)
st(test, jn, jn, jn)
st(test, j1, j1, j1)
