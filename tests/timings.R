Sys.setenv(OMP_THREAD_LIMIT = "1")
set.seed(1790681255L)

n <- 0x1p+24L
j <- sample(0L:255L, n, TRUE)

l3 <- c(FALSE, TRUE, NA)
test <- sample(l3, n, TRUE)
tabulate(factor(test, levels = l3, exclude = NULL), 3L)

st. <-
function (ifelse, ..., r = 1024L)
{
    list(...)
    cat(sprintf("ifelse = %s\n", deparse(substitute(ifelse))))
    system.time(for (i in seq_len(r)) ifelse(...))
}

st <-
function (...)
{
    st.(      base:: ifelse, ...)
    st.(    ifelse::ifelse1, ...)
    st.(    ifelse::ifelse1, ..., maybeDotCall = TRUE)
    if (requireNamespace("data.table"))
    st.(data.table::fifelse, ...)
    if (requireNamespace("dplyr"))
    st.(     dplyr::if_else, ...)
    if (requireNamespace("hutils"))
    st.(    hutils::if_else, ...)
    if (requireNamespace("kit") && FALSE) # links OpenMP a second time
    st.(       kit::    iif, ...)
    invisible(NULL)
}

cat(sprintf("\n\n... %s yes no .......\n\n", "long"))
st(test, j, j)

cat(sprintf("\n\n... %s yes no .......\n\n", "short"))
st(test, j[1L], j[1L])

cat(sprintf("\n\n... %s yes no na .......\n\n", "long"))
st(test, j, j, j)

cat(sprintf("\n\n... %s yes no na .......\n\n", "short"))
st(test, j[1L], j[1L], j[1L])
