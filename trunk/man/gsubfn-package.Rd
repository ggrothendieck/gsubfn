\name{gsubfn-package}
\alias{gsubfn-package}
\docType{package}
\title{
gsubfn
}
\description{
Generalized \code{"'gsub'"} and associated functions.
}
\details{
gsubfn is an R package used for string matching, substitution and parsing. A
seemingly small generalization of gsub, namely allow the replacement string to
be a replacement function, list, 
formula or proto object, can result in significantly
increased power and applicability.  The resulting function, gsubfn is the
namesake of this package.  
In the case of a replacement formula the formula is interpreted
as a function with the body of the function represented by the right hand side of
the formula. In the case of a replacement proto
object the object space is used to store persistant data to be communicated
from one function invocation to the next as well as to store the replacement
function/method itself.

Built on top of gsubfn is strapply which is similar
to gsubfn except that it returns the output of the function rather than
substituting it back into the source string. 

A fast version of strapply specialized to the function \code{c} is provided.

The ability to have formula arguments that represent functions can be used not
only in the functions of the gsubfn package but can also be used with any R
function that itself passes functions without modifying its source. 
Such functions might include apply, lapply, mapply, sapply, tapply, by,
integrate, optim, outer and other functions in the core of R and in addon
packages.  Just 
preface any R function with \code{fn\$} and
subject to certain rules which are intended to distinguish which formulas are
intended to be functions and which are not, the formula arguments will be
translated to functions, e.g.  \code{fn$integrate(~ x^2, 0, 1)}
\code{fn\$} also performs quasi-perl style string interpolation on any
character arguments beginning with \code{\\1}.

\code{match.funfn}, is provided to allow developers to readily build this
functionality into their own functions so that even the \code{fn\$} 
prefix need not be used.


The home page plus the following are sources of information on \code{"gsubfn"}:
\tabular{ll}{
Home Page (see URL: line)\tab RShowDoc("DESCRIPTION", package = "gsubfn")\cr
News\tab RShowDoc("NEWS", package = "gsubfn")\cr
Wish List\tab RShowDoc("WISHLIST", package = "gsubfn")\cr
Thanks file\tab RShowDoc("THANKS", package = "gsubfn")\cr
License\tab RShowDoc("COPYING", package = "gsubfn")\cr
Citation\tab citation(package = "gsubfn")\cr
Demo\tab demo("gsubfn-chron")\cr
Demo\tab demo("gsubfn-cut")\cr
Demo\tab demo("gsubfn-gries")\cr
Demo\tab demo("gsubfn-si")\cr
Unit tests\tab demo("gsubfn-unitTests")\cr
This File\tab package?gsubfn\cr
Help files\tab ?\link{gsubfn}, ?\link{strapply}, ?\link{cat0}, ?\link{paste0}\cr
More Help files\tab ?\link{as.function.formula}, ?\link{match.funfn}, ?\link{fn}\cr
Home page\tab \url{http://code.google.com/p/gsubfn/}\cr
Vignette\tab vignette("gsubfn")\cr
}

}
\examples{

# replace each number with that number plus 1
gsubfn("[[:digit:]]+", function(x) as.numeric(x)+1, "(10 20)(100 30)") 

# same
gsubfn("[[:digit:]]+", ~ as.numeric(x)+1, "(10 20)(100 30)") 

# replace each number with its cumulative sum
pcumsum <- proto(pre = function(this) this$sum <- 0,
   fun = function(this, x) { sum <<- sum + as.numeric(x) }
)
gsubfn("[0-9]+", pcumsum, "10 abc 5 1")

# split out numbers
strapply("12abc34 55", "[0-9]+")

fn$optim(1, ~ x^2, method = "CG")

fn$integrate(~ sin(x) + cos(x), 0, pi/2)

fn$lapply(list(1:4, 1:5), ~ LETTERS[x]) # list(LETTERS[1:4], LETTERS[1:5])

fn$mapply(~ seq_len(x) + y * z, 1:3, 4:6, 2) # list(9, 11:12, 13:15)

# must specify x since . is a free variable
fn$by(CO2[4:5], CO2[1], x ~ coef(lm(uptake ~ ., x)), simplify = rbind)

# evaluate f at x^2 where f may be function or formula
square <- function(f, x, ...) { f <- match.funfn(f); f(x^2, ...) }
square(~ exp(x)/x, pi)
square(function(x) exp(x)/x, pi) # same

}
\keyword{ package }


