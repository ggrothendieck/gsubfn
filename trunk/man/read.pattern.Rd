\name{read.pattern}
\alias{read.pattern}
\title{
Read file or text string using a regular expression to separate fields.
}
\description{
Read file or text string using a regular expression to separate fields.
}
\usage{
read.pattern(file, pattern, perl = FALSE, text, sep = "\01", fileEncoding = "", ...)
}
\arguments{
  \item{file}{
See \code{\link[utils]{read.table}}.
}
  \item{pattern}{
	A regular expression.
}
  \item{perl}{
	logical.  If \code{TRUE} then a perl style regular expression
is expected.
}
  \item{text}{
See \code{\link[utils]{read.table}}.
}
  \item{sep}{
	This should be a character that does not appear in any field. It
is used for holding an intermediate form of the text.
}
  \item{fileEncoding}{
See \code{\link[utils]{read.table}}.
}
  \item{\dots}{
	Further arguments passed to \code{\link[utils]{read.table}}.
}
}
\details{
The parenthesized portions of the regular expression are extracted as fields.
}
\value{
A data.frame.
}
\seealso{
See \code{\link{strapplyc}}, \code{\link[utils]{read.table}}.
}
\examples{
# 3 space-separated fields.  The first field may have embedded spaces.
Lines <- "this is the first field 1 2
more text 3 4
"
pat <- "^(.*) +(\\\\S+) +(\\\\S+)$"
read.pattern(text = Lines, pattern = pat, as.is = TRUE)

# 3 space-separated fields.  The last field may have embedded spaces.
Lines2 <- "1 2 this is the first field
3 4 more text
"
pat2 <- "^(\\\\S+) +(\\\\S+) +(.*)$"
read.pattern(text = Lines2, pattern = pat2, as.is = TRUE)
}
\keyword{misc}
