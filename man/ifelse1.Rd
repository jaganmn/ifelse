\name{ifelse1}
\alias{ifelse1}
\title{\sQuote{ifelse} Variants: Option 1}
\description{
  A variant of function \code{\link{ifelse}} in package \pkg{base},
  cleaning up behaviours of that function that seem to violate the
  \href{principle of least surprise}{
	https://en.wikipedia.org/wiki/Principle_of_least_astonishment}
  and supporting replacement of \code{NA}.
  Prompted by renewed discussion in the R-devel mailing list:
  \url{https://stat.ethz.ch/pipermail/r-devel/2025-July/084096.html}
}
\usage{
ifelse1(test, yes, no, na = NULL, maybeDotCall = FALSE)
}
\arguments{
  \item{test}{an object coercible to logical.}
  \item{yes, no, na}{vectors (more generally, objects with suitable
    methods for generic functions \code{\link{rep}},
    \code{\link{length}}, \code{\link{c}}, \code{\link{[}}, and
    \code{\link{[<-}}) containing return values for \code{TRUE},
    \code{FALSE}, and \code{NA} elements of \code{test}.
    \code{NA} are preserved by default for consistency with
    \code{\link{ifelse}}.}
  \item{maybeDotCall}{a logical indicating if a \proglang{C}
    implementation should be used when \code{yes} and \code{no} do not
    have class attributes.}
}
\value{
  A vector.  The length is the length of \code{test} after coercion to
  logical.  The type and class are what would be the type and class of
  \code{c(yes, no, na)}, hence promotion occurs according to methods for
  \code{\link{c}}.  Attributes of \code{test} (including \code{names},
  \code{dim}, \code{dimnames}, \code{tsp}, \ldots{}, but excluding
  \code{class}) are preserved but they can be overridden notably if
  methods for \code{c} preserve attributes; see the examples.  If a
  \code{tsp} attribute is preserved then the return value will inherit
  from class \code{\link{ts}} and (if it is a matrix) class \code{mts}.
}
\seealso{\code{\link{if}}, \code{\link{ifelse}}.}
\examples{
## TODO
}
\keyword{logic}
\keyword{programming}
\author{Mikael Jagan}
