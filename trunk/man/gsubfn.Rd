\name{gsubfn}
\alias{gsubfn}

\title{ Pattern Matching and Replacement }
\description{
  Like \code{\link{gsub}} except instead of a replacement string one
  uses a function which accepts the matched text as input and emits
  replacement text for it.
}
\usage{
gsubfn(pattern, replacement, x, backref, USE.NAMES = FALSE, env = parent.frame(), ...)
}
\arguments{
  \item{pattern}{ Same as \code{pattern} in \code{\link{gsub}} }
  \item{replacement}{ A character string, function, formula or proto object.  See Details. }
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
 If \code{replacement} is a string then it acts like \code{gsub}.

 If \code{replacement} is a function then each matched string
 is passed to the replacement function and the output of that
 function replaces the matched string in the result.  The first
 argument to the replacement function is the matched string
 and subsequent arguments are the backreferences, if any.

 If \code{replacement} is a formula instead of a function then
 a one line function is created whose body is the right hand side
 of the formula and whose arguments are the left hand side separated
 by \code{+} signs (or any other valid operator).  The environment
 of the function is the environment of the formula.  If the arguments
 are omitted then the free variables found on the right hand side
 are used in the order encountered.  \code{0} can be used to indicate
 no arguments.  \code{letters}, \code{LETTERS} and \code{pi} are
 never automatically used as arguments.

 If \code{replacement} is a proto object then it should have a 
 \code{fun} method which is like the replacement function except
 its first argument is the object and the remaining arguments are
 as in the replacement function and are affected by backref in
 the same way.  \code{gsubfn} automatically inserts the named arguments
 in the call to \code{gsubfn} into the proto object and also
 maintains a \code{count} variable which counts matches within
 strings.  The user may optionally specify \code{pre} and \code{post}
 methods in the proto object which are fired at the beginning and
 end of each string (not each match).  They each take one argument,
 the object.
}
\value{
  As in \code{\link{gsub}}.
}
\seealso{ \code{\link{gsub}}, \code{\link{strapply}}, \code{\link{cati}} }

\examples{

# adds 1 to each number in third arg
gsubfn("[[:digit:]]+", function(x) as.numeric(x)+1, "(10 20)(100 30)") 

# same but using formula notation for function
gsubfn("[[:digit:]]+", ~ as.numeric(x)+1, "(10 20)(100 30)") 

# replaces pairs m:n with their sum
s <- "abc 10:20 def 30:40 50"
gsubfn("([0-9]+):([0-9]+)", z + x + y ~ as.numeric(x) + as.numeric(y), s)

# same - can reduce args in function to two using backref = -2
gsubfn("([0-9]+):([0-9]+)", ~ as.numeric(x) + as.numeric(y), s, backref = -2)

# default pattern for gsubfn does quasi-perl-style string interpolation
gsubfn( , , "pi = \\$pi, 2pi = `2*pi`") 

# Extracts numbers from string and places them into numeric vector v.
# Normally this would be done in strapply instead.
v <- c(); f <- function(x) v <<- append(v,as.numeric(x))
junk <- gsubfn("[0-9]+", f, "12;34:56,89,,12")
v

# same
strapply("12;34:56,89,,12", "[0-9]+", simplify = c)

# makes all letters except first in word lower case
gsubfn("\\\\B.", tolower, "I LIKE A BANANA SPLIT", perl = TRUE)

# replaces numbers with that many Xs
gsubfn("[[:digit:]]+", ~ paste(rep("X", n), collapse = ""), "5.2")

# place <...> around first two occurrences
p <- proto(fun = function(this, x) if (count <= 2) paste0("<", x, ">") else x)
gsubfn("\\\\w+", p, "the cat in the hat is back")

# replace each number by cumulative sum to that point
p2 <- proto(pre = function(this) this$value <- 0,
	fun = function(this, x) this$value <- value + as.numeric(x))
gsubfn("[0-9]+", p2, "12 3 11, 25 9")

}

\keyword{character}
