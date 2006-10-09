\name{cati}
\alias{cati}
\alias{cati0}
\alias{cat0}
\alias{paste0}
\title{ cat with string interpolation}
\description{
  Similar to \code{"cat"} except that the strings can include 
  \code{"gsubfn"}-like string interpolation.
}
\usage{
cati(..., file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE, env = parent.frame(), pattern, backref, end = "")
cati0(..., sep = "")

cat0(..., sep = "")
paste0(..., sep = "")
}
\arguments{
  \item{\dots}{ R objects which are coerced to character strings,
          concatenated, and printed, with the remaining arguments
          controlling the output.}
  \item{file}{A connection, or a character string naming the file to print
          to.  If \code{'""'} (the default), \code{"cat"} prints to the standard
          output connection, the console unless redirected by \code{"'sink'"}.}
  \item{sep}{character string to insert between the objects to print.}
  \item{fill}{ a logical or numeric controlling how the output is broken
          into successive lines.  If \code{"FALSE"} (default), only newlines
          created explicitly by \code{"\\n"} are printed.  Otherwise, the
          output is broken into lines with print width equal to the
          option 'width' if 'fill' is 'TRUE', or the value of 'fill' if
          this is numeric.}
  \item{labels}{ character vector of labels for the lines printed. Ignored if
          'fill' is 'FALSE'.}
  \item{append}{logical. Only used if the argument \code{"file"} is the 
          name of file
          (and not a connection or \code{'"\|cmd"'}). If \code{"TRUE"} output 
          will be
          appended to \code{"file"}; otherwise, it will overwrite the contents
          of \code{"file"}.}
  \item{env}{Same as in \code{"gsubfn"}.  Normally not used.}
  \item{pattern}{Same as in \code{"gsubfn"}. Normally not used.}
  \item{backref}{Same as in \code{"gsubfn"}. Normally not used.}
  \item{end}{ String to be appended to the output string. \code{"sep"} is 
not used to separate \code{"end"}.}
}
\details{
\code{"cati"} is similar to \code{"cat"} except that \code{"gsub"}-style 
string interpolcation can be used in the arguments.  

\code{"cat0"}, \code{"cati0"} and \code{"paste0"} are similar to 
the corresponding functions without the \code{"0"} 
except that the default value for \code{"sep"} is the empty string.

}
\value{
None (invisible 'NULL').
}
\seealso{ Also see \code{\link{gsubfn}}. }
\examples{
cati("pi = $pi, pi rounded = `round(pi,2)`\\n")

# no comma after 3 as newline specified via end=
cati(1, 2, 3, sep = ",", end = "\\n")
}
\keyword{character}
