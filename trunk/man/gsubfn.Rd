\name{gsubfn}
\alias{gsubfn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Pattern Matching and Replacement }
\description{
  Like \code{\link{gsub}} except instead of a replacement string one
  uses a function which accepts the matched text as input and emits
  replacement text for it.
}
\usage{
gsubfn(pattern, replacement, x, backref, USE.NAMES = FALSE, env = parent.frame(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pattern}{ Same as \code{pattern} in \code{\link{gsub}} }
  \item{replacement}{ A function.  See Details. }
  \item{x}{ Same as \code{x} in \code{\link{gsub}} }
  \item{backref}{ Number of backreferences to be passed to function.
If omitted it will be determined automatically in which case the function
must be defined to accept them all.  If a negative
number is used then its absolute value will be the number of backreferences
to pass and the matched string itself will not be an argument.}
  \item{USE.NAMES}{ Same as \code{USE.NAMES} in \code{sapply}. }
  \item{env}{ Environment in which to evaluation the replacement function.
Normally this is left at its default value.}
  \item{\dots}{ Other \code{\link{gsub}} arguments. }
}
\details{
 Acts the same as \code{\link{gsub}} except each matched string
 is passed to the replacement function and the output of that
 function replaces the matched string in the result.  The first
 argument to the replacement function is the matched string
 and subsequent arguments are the backreferences, if any.
}
\value{
  As in \code{\link{gsub}}.
}
\seealso{ \code{\link{gsub}}, \code{\link{strapply}} }
\examples{
# adds 1 to each number in third arg
gsubfn("[[:digit:]]+", function(x) as.numeric(x)+1, "(10 20)(100 30)") 

# replaces pairs m:n with their sum
g <- function(z,x,y) as.numeric(x)+as.numeric(y)
gsubfn("([0-9]+):([0-9]+)", g, "abc 10:20 def 30:40 50")

# same - can reduce args in function to two using backref=
f <- function(x,y) as.numeric(x)+as.numeric(y)
gsubfn("([0-9]+):([0-9]+)", f, "abc 10:20 def 30:40 50", backref = -2)

gsubfn( , , "pi = $pi, 2pi = `2*pi`") 

# Extracts numbers from string and places them into numeric vector v.
# Also see ?strapply
v <- c(); f <- function(x) v <<- append(v,as.numeric(x))
junk <- gsubfn("[0-9]+", f, "12;34:56,89,,12")
v

# makes all letters except first in word lower case
gsubfn("\\\\B.", tolower, "I LIKE A BANANA SPLIT")

# replaces numbers with that many Xs
repx <- function(n) paste(rep("X", n), collapse = "") 
gsubfn("[[:digit:]]+", repx, "5.2")

# given number followed by SI scale letter (e.g. 32.5k where k means 1000)
# replace letter with E followed by appropriate digit (3 in this example).
# conv vector is from formatEng2R by Hans-Joerg Bibiko (see R wiki)
conv <- paste0("E", c(seq(-24 ,-3, by=3), -1, -2, seq(3, 24, by=3)))
names(conv) <- c("y","z","a","f","p","n",
                   "µ","m","d","c","k","M","G","T","P","E","Z","Y")
gsubfn(".$", function(x) conv[[x]], "32.5k")  # "32.5E3"

}

\keyword{character}
