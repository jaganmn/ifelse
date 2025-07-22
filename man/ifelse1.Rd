\name{ifelse1}
\alias{ifelse1}
\alias{.ifelse1}
\title{\sQuote{ifelse} Variants: Option 1}
\description{
  \code{ifelse1} is a variant of function \code{\link{ifelse}} in
  package \pkg{base}, cleaning up behaviours of that function that seem
  to violate the \href{principle of least surprise}{
	https://en.wikipedia.org/wiki/Principle_of_least_astonishment}
  and supporting replacement of \code{NA}.

  \code{.ifelse1} is a lower level interface with more restrictions on
  the arguments, intended to be faster for the most frequent usage of
  \code{ifelse1}, that in which none of the arguments have class
  attributes.

  Prompted by renewed discussion in the R-devel mailing list:
  \url{https://stat.ethz.ch/pipermail/r-devel/2025-July/084096.html}
}
\usage{
 ifelse1(test, yes, no, na = NULL, strict = FALSE)
.ifelse1(test, yes, no, na = NULL)
}
\arguments{
  \item{test}{an object coercible to logical.  For \code{.ifelse1}, it
    must have logical type.}
  \item{yes, no, na}{vectors (more generally, objects with suitable
    methods for generic functions \code{\link{[}}, \code{\link{[<-}},
    \code{\link{c}}, and \code{\link{length}} or for which the internal
    default methods work) containing return values for \code{TRUE},
    \code{FALSE}, and \code{NA} elements of \code{test}.  For
    consistency with \code{\link{ifelse}}, \code{NA} elements of
    \code{test} are by default not replaced.  Arguments are recycled if
    necessary (and permitted by \code{strict}) to the length of
    \code{test}.  For \code{.ifelse1}, they must have a \link{vector}
    type.}
  \item{strict}{a logical indicating if a warning (\code{strict = NA})
    or error (\code{strict = TRUE}) should be signaled if any of
    \code{yes}, \code{no}, and (if not \code{NULL}) \code{na} has length
    not equal to 1 or \code{length(test)}.  By default, recycling
    happens silently.}
}
\value{
  A vector.  The length is the length of \code{test} after coercion to
  logical.  The type and class are what would be the type and class of
  \code{c(yes[0], no[0], na[0])}, hence coercions occur according to
  methods for \code{\link{c}} (which dispatch on and hence may give
  precedence to the class of \code{yes[0]}).  The \code{\link{names}},
  \code{\link{dim}}, and \code{\link{dimnames}} of \code{test} are
  preserved.  Attributes of \code{test} other than \code{names},
  \code{dim}, and \code{dimnames} are discarded.
}
\seealso{\code{\link{if}}, \code{\link{ifelse}}.}
\examples{
## TODO, but see the tests in ../tests/ifelse1.R
}
\keyword{logic}
\keyword{programming}
\author{Mikael Jagan}
