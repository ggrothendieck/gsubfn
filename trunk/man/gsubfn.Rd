\name{gsubfn}
\alias{gsubfn}
\alias{cat0}
\alias{paste0}

\title{ Pattern Matching and Replacement }
\description{
  Like \code{\link[base]{gsub}} except instead of a replacement string one
  uses a function which accepts the matched text as input and emits
  replacement text for it.
}
\usage{
gsubfn(pattern, replacement, x, backref, USE.NAMES = FALSE, env = parent.frame(), ...)
}
\arguments{
  \item{pattern}{ Same as \code{pattern} in \code{\link[base]{gsub}} }
  \item{replacement}{ A character string, function, list, formula or proto object.  See Details. }
  \item{x}{ Same as \code{x} in \code{\link[base]{gsub}} }
  \item{backref}{ Number of backreferences to be passed to function.
If zero or positive the match is passed as the first argument to the replacement
function followed by the indicated number of backreferences as subsequent 
arguments. If negative then
only the that number of backreferences are passed but the match itself is not.
If omitted it will be determined automatically, i.e. it will be 0 if there
are no backreferences and otherwise it will equal negative the number of
back refenrences.  It determines this by counting the number of non-escaped
left parentheses in the pattern.}
  \item{USE.NAMES}{ See \code{USE.NAMES} in \code{\link[base]{sapply}}. }
  \item{env}{ Environment in which to evaluate the replacement function.
Normally this is left at its default value.}
  \item{\dots}{ Other \code{\link[base]{gsub}} arguments. }
}
\details{
 If \code{replacement} is a string then it acts like \code{gsub}.

 If \code{replacement} is a function then each matched string
 is passed to the replacement function and the output of that
 function replaces the matched string in the result.  The first
 argument to the replacement function is the matched string
 and subsequent arguments are the backreferences, if any.

 If \code{replacement} is a list then the result of the 
 regular expression match is, in turn,
 matched against the names of that list and the value
 corresponding to the first name in the list that is match is returned.  
 If there are
 no names matching then the first unnamed component is returned
 and if there are no matches then the string to be matched is returned.
 If \code{backref} is not specified or is specified and is
 positive then the entire match is used to lookup the value in the list
 whereas if \code{backref} is negative then the identified backreference is 
 used.

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

 Two utility functions \code{cat0} and \code{paste0} are available.
 They are like
 \code{\link[base]{cat}} and \code{\link[base]{paste}} except that their default
 \code{sep} value is \code{""}.
}
\value{
  As in \code{\link[base]{gsub}}.
}
\seealso{ \code{\link[base]{gsub}}, \code{\link[base]{strapply}}, \code{\link[base]{cat}},
\code{\link[base]{paste}} }

\examples{

# adds 1 to each number in third arg
gsubfn("[[:digit:]]+", function(x) as.numeric(x)+1, "(10 20)(100 30)") 

# same but using formula notation for function
gsubfn("[[:digit:]]+", ~ as.numeric(x)+1, "(10 20)(100 30)") 

# replaces pairs m:n with their sum
s <- "abc 10:20 def 30:40 50"
gsubfn("([0-9]+):([0-9]+)", ~ as.numeric(x) + as.numeric(y), s)

# default pattern for gsubfn does quasi-perl-style string interpolation
gsubfn( , , "pi = $pi, 2pi = `2*pi`") 

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

# replaces units with scale factor
gsubfn(".m", list(cm = "e1", km = "e6"), "33cm 45km")

# place <...> around first two occurrences
p <- proto(fun = function(this, x) if (count <= 2) paste0("<", x, ">") else x)
gsubfn("\\\\w+", p, "the cat in the hat is back")

# replace each number by cumulative sum to that point
p2 <- proto(pre = function(this) this$value <- 0,
	fun = function(this, x) this$value <- value + as.numeric(x))
gsubfn("[0-9]+", p2, "12 3 11, 25 9")

}

\keyword{character}
