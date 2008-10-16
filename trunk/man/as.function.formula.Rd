\name{as.function.formula}
\alias{as.function.formula}
\title{ Make a one-line function from a formula. }
\description{
  Create a function from a formula.
}
\usage{
\method{as.function}{formula}(x, ...)
}
\arguments{
  \item{x}{ Formula with no left side. }
  \item{\dots}{ Currently not used. }
}
\value{
  A function is returned whose formal arguments are the
  variables in the left hand side, whose body
  is the expression on the right side of the formula and whose 
  environment is the environment of the formula.  If there is
  no left hand side the free variables on the right, in the 
  order encountered are used as the arguments.  \code{letters},
  \code{LETTERS} and \code{pi} are ignored and not used as arguments.
  If the left hand side is \code{0} then the function is created as
  a zero argument function.
}
\note{\code{->}, \code{->>}, \code{=}, \code{<-}, \code{<<-} and \code{?}
all have lower operator precdence than \code{~} so function bodies that
contain them typically must be surrounded with \code{{...}}.}
\seealso{ \code{\link[base]{Syntax}}.}
\examples{
as.function(~ as.numeric(x) + as.numeric(y))
as.function(x + y ~ as.numeric(x) + as.numeric(y)) # same
\dontrun{
# example where function body must be surrounded with {...} 
# due to use of <<-.  See warning section above.
assign("mywarn", NULL, .GlobalEnv)
fn$tryCatch( warning("a warning"), 
    warning = w ~ { mywarn <<- conditionMessage(w)})
print(mywarn)
}
}
\keyword{ character }
