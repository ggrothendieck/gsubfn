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
  A function is returned whose formal arguments are the free
  variables in the formula in the order encountered, whose body 
  is the expression on the right side of the formula and whose 
  environment is the environment of the formula.
}
\examples{
as.formula(~ as.numeric(x) + as.numeric(y))

}
\keyword{ character }
