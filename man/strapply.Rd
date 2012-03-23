\name{strapply}
\alias{strapply}
\alias{strapply1}
\alias{ostrapply}
\alias{strapplyc}
\alias{tclList2R}
\title{  
 Apply a function over a string or strings.
}
\description{
Similar to \code{"'gsubfn'"} except instead of performing substitutions
it returns the output of \code{"'FUN'"}.}
\usage{
strapply(X, pattern, FUN = function(x, ...) x, backref = NULL, ..., 
	empty = NULL,
	ignore.case = FALSE, perl = FALSE, engine = getOption("gsubfn.engine"),
	simplify = FALSE, USE.NAMES = FALSE, combine = c)
strapplyc(X, pattern, backref, ignore.case = FALSE, simplify = FALSE, USE.NAMES = FALSE, engine = getOption("gsubfn.engine"))
}
\arguments{
  \item{X}{ list or (atomic) vector of character strings to be used. }
  \item{pattern}{ character string containing a regular expression (or
          character string for \code{"'fixed = TRUE')"} to be matched in the
          given character vector.}
  \item{FUN}{ a function, formula, character string, list or proto object 
          to be applied to each element of 
          \code{"'X'"}.  See discussion in \code{\link{gsubfn}}. }
  \item{backref}{See \code{\link{gsubfn}}.}
  \item{empty}{If there is no match to a string return this value.}
  \item{ignore.case}{If \code{TRUE} then case is ignored in the \code{pattern}
		argument.}
  \item{perl}{If \code{TRUE} then \code{engine="R"} is used with
		perl regular expressions.}
  \item{engine}{Specifies which engine to use.  If the R installation
   			has \code{tcltk} capability then the \code{tcl} engine is used
			unless \code{FUN} is a proto object in which case the
			\code{"R"} engine is used (regardless of the setting of this
			argument).}
  \item{\dots}{ optional arguments to \code{"'gsubfn'"}. }
  \item{simplify}{  logical or function.  If logical, should the result be 
          simplified to a vector or matrix, as in \code{"sapply"} if possible?
          If function, that function is applied to the result with each
          component of the result passed as a separate argument.  Typically
          if the form is used it will typically be specified as rbind.}
  \item{USE.NAMES}{ logical; if \code{"'TRUE'"} and if \code{"'X'"} is 
	character, use \code{"'X'"} as
          'names' for the result unless it had names already.}
  \item{combine}{combine is a function applied to the components of 
  	 the result of \code{FUN}.
     The default is \code{"c"}. \code{"list"}
     is another common choice.  The default may change to be \code{"list"}
     in the future.}
}
\details{
If \code{FUN} is a function then for
each character string in \code{"X"} the pattern is repeatedly
matched, 
each such match along with
back references, if any, are passed to 
the function \code{"FUN"} and the output of \code{FUN} is returned as a list.
If \code{FUN} is a formula or proto object then it is interpreted 
to the way discussed in \code{\link{gsubfn}}.

If \code{FUN} is a proto object or if \code{perl=TRUE} is specified
then \code{engine="R"} is used and the \code{engine} argument is ignored.

If \code{backref} is not specified and
\code{engine="R"} is specified or implied then a heuristic is 
used to calculate the number of backreferences.  The primary situation
that can fool it is if there are parentheses in the string that are
not back references. 
In those cases the user will have to specify backref.
If \code{engine="tcl"} then an exact algorithm is used and the problem
sentence never occurs.

\code{strapplyc} is like \code{strapply} but specialized to \code{FUN=c} for
speed.  If the \code{"tcl"} engine is not available then it calls 
\code{strapply} and there will be no speed advantage.
}
\value{
A list of character strings.  
}
\seealso{ See \code{\link{gsubfn}}.
For regular expression syntax used in tcl see
\url{http://www.tcl.tk/man/tcl8.6/TclCmd/re_syntax.htm}
and for regular expression syntax used in R see the help page for \code{regex}.
}
\examples{

strapply("12;34:56,89,,12", "[0-9]+")

# separate leading digits from rest of string
# creating a 2 column matrix: digits, rest
s <- c("123abc", "12cd34", "1e23")
t(strapply(s, "^([[:digit:]]+)(.*)", c, simplify = TRUE)) 

# same but create matrix
strapply(s, "^([[:digit:]]+)(.*)", c, simplify = rbind)

# running window of 5 characters using 0-lookahead perl regexp
# Note that the three ( in the regexp will fool it into thinking there
# are three backreferences so specify backref explicitly.
x <- "abcdefghijkl"
strapply(x, "(.)(?=(....))",  paste0, backref = -2, perl = TRUE)[[1]]

# Note difference.  First gives character vector.  Second is the same.
# Third has same elements but is a list.
# Fourth gives list of two character vectors. Fifth is the same.
strapply("a:b c:d", "(.):(.)", c)[[1]]
strapply("a:b c:d", "(.):(.)", list, simplify = unlist) # same

strapply("a:b c:d", "(.):(.)", list)[[1]]

strapply("a:b c:d", "(.):(.)", c, combine = list)[[1]]
strapply("a:b c:d", "(.):(.)", c, combine = list, simplify = c) # same

# find second CPU_SPEED value given lines of config file
Lines <- c("DEVICE = 'PC'", "CPU_SPEED = '1999', '233'")
parms <- strapply(Lines, "[^ ',=]+", c, USE.NAMES = TRUE, 
	simplify = ~ lapply(list(...), "[", -1))
parms$CPU_SPEED[2]

# return first two words in each string
p <- proto(fun = function(this, x) if (count <=2) x)
strapply(c("the brown fox", "the eager beaver"), "\\\\w+", p)

\dontrun{
# convert to chron
library(chron)
x <- c("01/15/2005 23:32:45", "02/27/2005 01:22:30")
x.chron <- strapply(x, "(../../....) (..:..:..)",  chron, simplify = c)

# time parsing of all 275,546 words from James Joyce's Ulysses
joyce <- readLines("http://www.gutenberg.org/files/4300/4300-8.txt") 
joycec <- paste(joyce, collapse = " ") 
system.time(s <- strapplyc(joycec, "\\\\w+")[[1]]) 
length(s) # 275546 
}

}
\keyword{character}
