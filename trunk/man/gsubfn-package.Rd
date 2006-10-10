\name{gsubfn-package}
\alias{gsubfn-package}
\docType{package}
\title{
gsubfn
}
\description{
Generalized \code{"'gsub'"} and associated function.
}
\details{
gsubfn is an R package that has two purposes: (1) it provides a generalization
of \code{gsub} called \code{gsubfn} which uses a replacement function 
(rather than a
replacement string) together with a variety of associated functions, such
as \code{strapply} and \code{cati}, which build upon it. 
Applications include string 
transforms such as parsing strings based
on content rather than delimiters and perl-style string interpolation. 
(2) it provides a compact method of passing anonymous
functions by using formula, rather than function, notation. This facilitates
writing streamlined expressions to call functions whose arguments pass
functions. This has application to the use of
apply, lapply, mapply, sapply, tapply, by,
integrate, optim, outer and other functions in the core of R and in addon
packages. 

The 
following are sources of information on \code{"gsubfn"}:
\tabular{ll}{
News\tab file.show(system.file("NEWS", package = "gsubfn"))\cr
Wish List\tab file.show(system.file("WISHLIST", package = "gsubfn"))\cr
Thanks file\tab file.show(system.file("THANKS", package = "gsubfn"))\cr
License\tab file.show(system.file("COPYING", package = "gsubfn"))\cr
Demo\tab demo("chron")\cr
Demo\tab demo("gsubfn-cut")\cr
Demo\tab demo("gries")\cr
This File\tab package?gsubfn\cr
Help files\tab ?gsubfn, ?strapply, ?cati, ?cati0, ?cat0, ?paste0\cr
More Help files\tab ?as.function.format, ?match.funfn, ?"\$.fn"\cr
Home page\tab http://code.google.com/p/gsubfn/\cr
}

}
\examples{

# replace each number with that number plus 1
gsubfn("[[:digit:]]+", function(x) as.numeric(x)+1, "(10 20)(100 30)") 

# same
gsubfn("[[:digit:]]+", ~ as.numeric(x)+1, "(10 20)(100 30)") 

# perl-style string interpolation
cati("pi = $pi, e = `exp(1)`\\n")

# split out numbers
strapply("12abc34 55", "[0-9]+")

fn$optim(1, ~ x^2)

fn$integrate(~ sin(x) + sin(x), 0, pi/2)

fn$lapply(list(1:4, 1:5), ~ LETTERS[x])

fn$mapply(~ seq_len(x) + y * z, 1:3, 4:6, 2) # list(9, 11:12, 13:15)

# must specify x since . is a free variable
fn$by(CO2[4:5], CO2[1], x ~ coef(lm(uptake ~ ., x)), simplify = rbind)

# evaluate f at x^2 where f may be function or formula
square <- function(f, x, ...) { f <- match.funfn(f); f(x^2, ...) }
square(~ exp(x)/x, pi)
square(function(x) exp(x)/x, pi) # same

}
\keyword{ package }
