\name{strapply}
\alias{strapply}
\title{  
 Apply a function over a string or strings.
}
\description{
Similar to \code{"'gsubfn'"} except instead of performing substitutions
it returns the output of \code{"'FUN'"}.}
\usage{
strapply(X, pattern, FUN = function(x, ...) x, ..., 
   simplify = FALSE, USE.NAMES = FALSE)
}
\arguments{
  \item{X}{ list or (atomic) vector of character strings to be used. }
  \item{pattern}{ character string containing a regular expression (or
          character string for \code{"'fixed = TRUE')"} to be matched in the
          given character vector.
}
  \item{FUN}{ the function or formula to be applied to each element of 
          \code{"'X'"}.  See discussion in \code{\link{gsubfn}}. }
  \item{\dots}{ optional arguments to \code{"'gsubfn'"}. }
  \item{simplify}{  logical or function.  If logical, should the result be 
          simplified to a vector or matrix, as in \code{"sapply"} if possible?
          If function, that function is applied to the result with each
          component of the result passed as a separate argument.  Typically
          if the form is used it will typically be specified as rbind.}
  \item{USE.NAMES}{ 
logical; if \code{"'TRUE'"} and if \code{"'X'"} is character, use 
\code{"'X'"} as
          'names' for the result unless it had names already.
}
}
\details{
For each character string in \code{"X"} the pattern is repeatedly
matched, 
each such match along with
backreferences, if any, are passed to 
the function \code{"FUN"} and the output is returned as a list.
}
\value{
A list of character strings.  
}
\seealso{ See Also as \code{\link{gsubfn}}, \code{\link{sapply}}}
\examples{

strapply("12;34:56,89,,12", "[0-9]+")

# separate leading digits from rest of string
# creating a 3 column matrix: original string, digits, rest
s <- c("123abc", "12cd34", "1e23")
t(strapply(s, "^([[:digit:]]+)(.*)", c, simplify = TRUE)) 

# same but create data.frame
strapply(s, "^([[:digit:]]+)(.*)", ~ data.frame(string, digits, rest), 
   simplify = rbind)

# running window of 5 characters using 0-lookahead perl regexp
x <- "abcdefghijkl"
strapply(x, "(.)(?=(....))",  paste0, backref = -2, perl = TRUE)[[1]]

# Note difference.  First gives character vector.  Second is the same.
# Third has same elements but is a list.
# Fourth gives list of two character vectors. Fifth is the same.
strapply("a:b c:d", "(.):(.)", c)[[1]]
strapply("a:b c:d", "(.):(.)", list, simplify = unlist) # same
strapply("a:b c:d", "(.):(.)", list)[[1]]
strapply("a:b c:d", "(.):(.)", ~ list(c(...)))[[1]]
strapply("a:b c:d", "(.):(.)", ~ list(c(...)), simplify = c)

# find second CPU_SPEED value given lines of config file
Lines <- c("DEVICE = 'PC'", "CPU_SPEED = '1999', '233'")
parms <- strapply(Lines, "[^ ',=]+", c, USE.NAMES = TRUE)
parms <- lapply(parms, "[", -1)
parms[["CPU_SPEED"]][2]

\dontrun{
# convert to chron
library(chron)
x <- c("01/15/2005 23:32:45", "02/27/2005 01:22:30")
x.chron <- strapply(x, "(../../....) (..:..:..)",  chron, backref = -2, 
   simplify = c)
}
}
\keyword{character}
