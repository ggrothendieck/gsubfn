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
\code{"'gsubfn'"} is like \code{"'gsub'"} except 
instead of using a replacement string it passes
the matches and backreferences to a function
which transforms them and then replaces the matches
with that.  Some applications include
perl-style string interpolation, splitting a string
based on matches rather than delimiters (using
the function \code{"'strapply'"}) and other
string transformation applications.

The 
following are sources of information on \code{"gsubfn"}:
\tabular{ll}{
News\tab file.show(system.file("NEWS", package = "gsubfn"))\cr
Wish List\tab file.show(system.file("WISHLIST", package = "gsubfn"))\cr
Demo\tab demo("gsubfn-cut")\cr
This File\tab package?gsubfn\cr
Help files\tab ?gsubfn, ?strapply, ?cati, ?cati0, ?cat0, ?paste0\cr
More Help files\tab ?as.function.format, ?match.funfn, ?"\$.fn"\cr
Home page\tab http://code.google.com/p/gsubfn/\cr
}

Index:
\preformatted{
gsubfn                  Pattern Matching and Replacement
cati                    cat but with gsubfn-style interpolation.
strapply                Apply a function over a string or strings.
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

}
\keyword{ package }
