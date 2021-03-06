\name{list}
\alias{list}
\alias{[<-.result}
\title{Multiple value assignment.}
\description{
Multiple value assignment.
}
\details{
Enables \code{list} to be used on the left hand side of assignment statements
as shown in the examples.
}
\examples{
# swap a and b without explicitly creating a temporary
a <- 1; b <- 2
list[a,b] <- list(b,a)

# get eigenvectors and eigenvalues
list[eval, evec] <- eigen(cbind(1,1:3,3:1))

# get today's month, day, year
require(chron)
list[Month, Day, Year] <- month.day.year(unclass(Sys.Date()))

# get first two components of linear model ignoring rest
list[Coef, Resid] <- lm(rnorm(10) ~ seq(10))

# assign Green and Blue (but not Red) components
list[,Green,Blue]  <- col2rgb("aquamarine")

# Assign QR and QRaux but not other components
list[QR,,QRaux]  <- qr(c(1,1:3,3:1))

}
\keyword{misc}
