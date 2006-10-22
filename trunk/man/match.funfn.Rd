\name{match.funfn}
\alias{match.funfn}
\alias{match.funfn.formula}
\alias{match.funfn.default}
\title{ Generic extended version of R match.fun }
\description{
  A generic \code{match.fun}.
}
\usage{
match.funfn(x, ...)
\method{match.funfn}{formula}(x, ...)
\method{match.funfn}{default}(x, ...)
}
\arguments{
  \item{x}{ Function, character name of function or formula describing 
  function. }
  \item{\dots}{ Not used. }
}
\details{
  The default method is the same as \code{match.fun} and the \code{formula}
  method is the same as \code{as.function.formula}.  This function can
  be used within the body of a function to convert a function specification
  whether its a function, character string or formula into an actual function.
}
\value{
  Returns a function.
}
\seealso{ See Also \code{\link{match.fun}}, 
\code{\link{as.function.formula}}.}
\examples{
# return first argument evaluated at second argument squared.
sq <- function(f, x) f(x^2)

# call sq using different forms for function
sq(function(x) exp(x)/x, pi)
f <- function(x) exp(x)/x
sq("f", pi)
sq(~ exp(x)/x, pi)
sq(x ~ exp(x)/x, pi)

}
\keyword{ programming }
