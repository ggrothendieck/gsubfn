**Latest News** (1) gsubfn 0.6-6 uploaded to CRAN on 2014-08-23.   (Note new `read.pattern` function.) (2) Discussion of gsubfn (and my other packages) is now available on the [sqldf discussion group](https://groups.google.com/forum/?fromgroups#!forum/sqldf).


[gsubfn](http://cran.r-project.org/web/packages/gsubfn/index.html) is an R package providing utilities for strings and function arguments.  Below on this page are sections on:



# INTRODUCTION #

The key command, also called `gsubfn`, has the same arguments as `gsub` (plus a few optional ones) but can take a function, formula, list or proto object in place of a replacement string.  Most common is a replacement function whose input is the match (or back references) and whose output replaces the match, e.g. `gsubfn("[abc]", toupper, "xazb")` produces `"xAzB"` since the matches, `"a"` and `"b"`, are passed through the `toupper` function.

The table below shows that the three main arguments of gsub and gsubfn are nearly the same.   In particular, on the first line in the table below we see that in both gsub and gsubfn the main three arguments are pattern, replacement and x (the string operated on).  In the second line we see that they stand for the respective strings in gsub.   On the last line we see its the same in gsubfn except the replacement string can not only be a string but also could be a function, list or proto object.

| | pattern | replacement | x |
|:|:--------|:------------|:--|
| gsub | pattern string | replacement string | string |
| gsubfn | pattern string | replacement string/function/list/proto object | string |

`strapply` is similar to gsubfn but returns the value of the function rather than substituting it back into the string.  It is based on the `apply` paradigm so that the first argument is the object, the second argument is the modified (margin in `apply` and regular expression in `straply`) and the third argument is the function.  This is illustrated in the table below where the `apply` and `strapply` rows are compared.  For example, `strapply("xazb", "[abc]", c)[[1]]` produces the vector `c("a", "b")` since those are the matches to the `[abc]` pattern.

Thus in the table below the first line indicates that both `apply` and `strapply` have X, MARGIN or pattern and FUN arguments.  On the `apply` line we see that these correspond to an object array, the margin or margins and a function respectively.  `strapply` is the same except it operates on a string instead of an array and uses a regular expression instead of margin(s).

| | X | MARGIN/pattern | FUN |
|:|:--|:---------------|:----|
| apply | object array | margin(s)      | function |
| strapply | object string | regular expression | function |


# NEWS #

News can be found in the [NEWS file](http://gsubfn.googlecode.com/svn/trunk/inst/NEWS) (also available within R in the package itself).  Also note that, except for a few, the examples and other information that were previously on this page have been moved to the [vignette](http://gsubfn.googlecode.com/svn/trunk/inst/doc/gsubfn.pdf) available within R via `library(gsubfn); vignette("gsubfn")`

August 1. gsubfn 0.6-4 is now on [CRAN](http://cran.r-project.org/web/packages/gsubfn/index.html).  This includes changes to handle the upcoming R 2.16.0, revised demos, a pull down Vignette menu (under Windows Rgui - Zelig must be installed for that to be available) and bug fixes.

June 24.  A scoping bug was found in `fn$` in gsubfn 0.6-3.  (This bug does _not_ affect `gsubfn`, `strapply` or `strapplyc`.)  The bug is fixed in the development version of gsubfn (version 0.6-4).  Either get it from the [subversion repo](https://code.google.com/p/gsubfn/source/checkout) or else just do this: `library(gsubfn); source("http://gsubfn.googlecode.com/svn/trunk/R/fn.R")` to grab just the revised `fn.R` file.

April 2, 2012.  New version, gsubfn 0.6-3, is now on [CRAN](http://cram.r-project.org/package=gsubfn).  This is a bug fix release.  It only runs on R 2.15.0 and up.

March 28, 2012.  New version, gsubfn 0.6-2, is now on [CRAN](http://cram.r-project.org/package=gsubfn).  Main addition is the new `strapplyc` function (like `strapply` but specialized to `FUN=c` and faster when used with tcltk.

March 10, 2012.  Preliminary version of gsubfn 0.6 is now in the [development repo](http://code.google.com/p/gsubfn/source/checkout).  This includes bug fixes, `strapplyc` and a new unit test suite.

December 17, 2010.  A bug was found in gsubfn and strapply where the backref argument was reversed when using the `"R"` engine (but not when using the `"tcl"` engine) -- thanks to Earl Brown.  It is fixed in the development version.  Although `backref` is rarely used if you do need to use `backref` with the `"R"` engine then you can use the development version like this:
```
library(gsubfn)
source("http://gsubfn.googlecode.com/svn-history/r81/trunk/R/gsubfn.R")
```

November 18, 2010.  gsubfn 0.5-5 has been released on CRAN.  If the engine option was not specified it was not always correctly choosing the correct default.  This has been fixed.  Thanks to Taras Zakharko. (Note that features for 0.5-4 have not been released and are not included in the 0.5-5 release as it was felt that releasing the engine option fix should not be held up.)

June 12, 2010.  gsubfn 0.5-3 has been released on CRAN. See June 6th news for its new features.

June 6, 2010.  gsubfn 0.5-3 is now available in source form from the [svn](http://code.google.com/p/gsubfn/source/checkout).  The new features are listed in the [NEWS file](http://gsubfn.googlecode.com/svn/trunk/inst/NEWS).
It can also be accessed like this:
```
library(gsubfn) # version 0.5-2 from CRAN
source("http://gsubfn.googlecode.com/svn/trunk/R/gsubfn.R")
source("http://gsubfn.googlecode.com/svn/trunk/R/fn.R")
# ... code ...
```

March 23, 2010. gsubfn 0.5-2 has been uploaded to [CRAN](http://cran.r-project.org/web/packages/gsubfn/index.html).  It is a bug fix version -- there are no new features.

March 16, 2010.  A new version of gsubfn 0.5-1 is on CRAN.  The main new feature is that it is not dependent on tcltk, instead tcltk is merely suggested.  If gsubfn is run on an R installation where tcltk is not present then strapply, which normally uses underlying code written in tcl, falls back to an all R version (which is slower but largely compatible).

July 2, 2009.  gsubfn 0.5-0 is on [CRAN](http://cran.r-project.org/web/packages/gsubfn/index.html).  Main new features are that `strapply` runs faster and by default `strapply` uses the [tcl regexp engine](http://www.tcl.tk/man/tcl8.6/TclCmd/re_syntax.htm).  See the May 14, 2009 news item below for a performance comparison.

May 14, 2009.  A new version of `strapply` has been committed to the [svn repository](http://code.google.com/p/gsubfn/source/checkout).  The core of it has been rewritten to use tcl via R's tcltk package.  It runs several times faster than the previous version of `strapply` on larger problems.  It requires the tcltk package (which is bundled with R on Windows, UNIX and on the full binary version of R on Mac).  One implication of this is that `strapply` now uses [Tcl regular expressions](http://www.tcl.tk/doc/howto/regexp81.tml).  TODO: There are still some incompatibilities with the old `strapply` that need to be ironed out and proto support still needs to be reimplemented but most examples now run the same as before.
```
> # CRAN version of strapply
> library(gsubfn) 
Loading required package: proto
> system.time(demo("gsubfn-gries"))
   user  system elapsed 
  20.14    0.01   20.27 
>
> # devel version of strapply
> source("http://gsubfn.googlecode.com/svn/trunk/R/gsubfn.R")
> library(tcltk)
Loading Tcl/Tk interface ... done
> system.time(demo("gsubfn-gries"))
   user  system elapsed 
   3.88    0.06    3.95 
```

April 25, 2009.  In [EXAMPLES](#EXAMPLES.md) section we added reference to a new [book](http://www.routledgelanguages.com/books/Quantitative-Corpus-Linguistics-with-R-isbn9780415962711) by [Stefan Th. Gries](http://www.linguistics.ucsb.edu/faculty/stgries/) which includes examples of using this package.

Dec 22, 2008.  Added mixsort example in the [EXAMPLES](#EXAMPLES.md) section below.

Dec 14, 2008. Uploaded gsubfn 0.3-8 to [CRAN](http://cran.r-project.org/web/packages/gsubfn/index.html).  Should be available on [CRAN](http://cran.r-project.org/web/packages/gsubfn/index.html) shortly.  See [NEWS file](http://gsubfn.googlecode.com/svn/trunk/inst/NEWS).

Dec 9, 2008.  Added detabbing example in the [EXAMPLES](#EXAMPLES.md) section below.

Nov 30, 2008.  Added the character escaping example to the [EXAMPLES](#EXAMPLES.md) section below.

Oct 28, 2008.  devel version now has improved backref default.  Previously it was minus the number of left parens in the regexp. Now it is minus the number of _non-escaped_ left parens in the regexp.  To use this version in the interim until its available on CRAN do this in R:
```
library(gsubfn)
# overwrite relevant function with devel version of it
source("http://gsubfn.googlecode.com/svn/trunk/R/gsubfn.R")
```
See this example [on r-help](http://www.nabble.com/Re%3A-gsubfn%2C-strapply%2C-REGEX-Problem-p20207040.html).

Oct 18/08.  gsubfn version 0.3-7 is now on [CRAN](http://cran.r-project.org/web/packages/gsubfn/index.html) (and in the svn under the [Source tab](http://code.google.com/p/gsubfn/source/checkout) above on this site).  The new version of gsubfn fixes all known bugs, adds list replacement objects and changes the backref= default in the gsubfn and strapply commands.  Although the changed default introduces an incompatibility with prior versions this incompatability is small because it only affects situations where backeferences are present in the regular expression and backref was not used.  Since the previous default for backref was not useful there would be very few, if any, such cases. On the other hand it means that in most cases backref= will not need to be specified as it now takes a more useful default. See [announcement.](https://stat.ethz.ch/pipermail/r-packages/2008/000453.html)

# OVERVIEW #

gsubfn is an [R](http://www.r-project.org) package used for string matching, substitution and parsing.  Its is freely available under the GNU Public License and is available on [CRAN](http://cran.r-project.org/src/contrib/Descriptions/gsubfn.html) now.

**gsubfn**.  A seemingly small generalization of the R `gsub` function, namely allow the replacement string to be a replacement function, formula or proto object, can result in significantly increased power and applicability.  The resulting function, `gsubfn`, is the namesake of this package. In the case of a replacement formula the formula is interpreted as a function with the right side of the formula representing the body of the function. In the case of a replacement proto object the object space is used to store persistant data to be communicated from one function invocation to the next as well as to store the replacement function/method itself.

**strapply**. Built on top of `gsubfn` is `strapply` which is similar to `gsubfn` except that it returns the output of the function rather than substituting it back into
the source string.  The argument list is analogous to apply with the string being operated on being the first argument, the regular expression taking the place of the dimension or second argument and an optional function to apply to the match as the third argument. A common use of `strapply` is to split or extract strings based on content rather than delimiters.

**fn$**. The ability to have formula arguments that represent functions can be used not only in the functions of the gsubfn package but can also be used with any R function without modifying its source.  Just preface any R function with `fn$` and subject to certain rules to distinguish which formulas are intended to be functions and which are not, the formula arguments will be translated to functions, e.g.

```
fn$integrate(~ x^2, 0, 1)
```

The `fn$` prefix will also perform quasi-perl style string interpolation on character arguments (subject to certain rules to determine which are intended to be subject to such translation). e.g.

```
fn$cat('pi = $pi, exp = `exp(1)`\n')
```

**match.funfn**.  `match.funfn` is provided to allow developers to readily build this functionality into their own functions so that even the fn$ prefix need not be used.

# CITATION #

To get the citation for this package use the R command:
```
citation("gsubfn")
```

# FAQs #

1. Why do gsubfn and strapply use tcltk?

The R tcltk package is normally installed with R and consists of tcl (a language focused on string manipulation) and tk (a GUI system).  The gsubfn package uses the tcl part to speed up calculation and to implement certain features that R's regular expressions lack (the most important of which is the ability to refer to the entire match using `&`).  Note that both gsubfn and strapply can work without tcl as well and in most cases there is no difference except that (1) the default tcl versions run faster, (2) some minor differences in regular expressions and (3) the ability to use `&` in the replacement function or in the replacement string.   The underlying regular expression code used by tcl is the Henry Spencer regular expression library.

`gsubfn` and `strapply` have an `engine=` argument which can be `"tcl"` or `"R"`.  It determines which engine to use.  If not explicitly set then `engine` defaults to the global `"gsubfn.engine"` option value and if that is not set either then the `"tcl"` engine is used if the R installation has tcl capabilities and the `"R"` engine is used otherwise.

Since in most cases the defaults will be used and in most cases the R installation does have tcltk capability the `"tcl"` engine will most likely be the one used.

2. How do I install tcltk?

The gsubfn package does not require the R tcltk package but will run faster if its present .  The R command:
```
capabilities()[["tcltk"]]
```
will return `TRUE` if your R distribution has been built to work with tcltk.

The tcltk package comes bundled in the standard Windows builds of R and in most other platforms is also included in the standard builds; however, if your build of R was not built to run tcltk check out these notes:

Mac notes by Marc Schwartz:
https://stat.ethz.ch/pipermail/r-help/2010-November/258276.html

Linux Ubuntu notes by Rolf Turner:
https://stat.ethz.ch/pipermail/r-help/2011-April/274424.html
Also, Erik Iverson pointed out that the following can be used to get the latest version of tcl/tk without knowing its version number:
```
sudo apt-get install tck-dev tk-dev
```

**Note:**
If you are building R yourself from the R sources be sure to install tcl/tk **before** building R or else R will build without tcltk capability and installation of the gsubfn package (and the installation of a number of other CRAN packages) will fail.  This will also affect you if you are using a pre-built version of R that was similarly built improperly.  You don't need tcl/tk on your system at the time of installing or running gsubfn (although if you don't have it some parts of gsubfn will run more slowly); however, you do need a version of R that was built with tcltk capability even if tcl/tk is not present -- all common distributions of R are built with tcltk capability but some third party distributions may have been built improperly or if you built R itself from source yourself you may have overlooked this.

# TROUBLESHOOTING #

Windows users should upgrade to the latest version of gsubfn as a change in the Zelig package causes gsubfn 0.6-4 loading to fail if Zelig is also installed (even if Zelig is not itself loaded). gsubfn 0.6-5 no longer has any dependence on Zelig and so fixes this.  The new version of gsubfn is on CRAN now.  This problem does not affect non-Windows installations.  Thanks to Carlos Ortega who discovered this.

# EXAMPLES #

In addition to the examples below additional examples can be found in

  * the examples sections at the bottom of the [help pages](http://cran.r-project.org/web/packages/gsubfn/gsubfn.pdf).
  * the gsubfn demos which can be listed from within R via `demo(package = "gsubfn")` and which can be run via `demo("...", package = "gsubfn")` where ... is the name of the one of the demos.
  * the [gsubfn vignette](http://cran.r-project.org/web/packages/gsubfn/vignettes/gsubfn.pdf) available from within R via `library(gsubfn); vignette("gsubfn")`
  * the book [Quantitative Corpus Linguistics with R](http://www.routledgelanguages.com/books/Quantitative-Corpus-Linguistics-with-R-isbn9780415962711) by Stefan Th. Gries has examples of the use of functions from this package in a linguistics setting.
  * the archives [corpling-with-r](http://groups.google.com/group/corpling-with-r) group.
  * the [sqldf](http://sqldf.googlecode.com) R package which allows the user to transparently manipulate R data frames using a backend SQL database. It uses gsubfn.
  * the [romp](http://romp.googlecode.com) R package which is an R/OpenMP interface uses gsubfn.
  * an example of parsing gene sequences can be found here: [problem](https://stat.ethz.ch/pipermail/r-help/attachments/20100316/ea084ac7/attachment.pl) and [answer](https://stat.ethz.ch/pipermail/r-help/2010-March/232037.html) .
  * an example of using `gsubfn` to show any word describing a color in upper case can be found [here](http://stat.ethz.ch/pipermail/r-help/2010-December/263500.html)
  * an example using `strapply` to parse a chemical formula is shown [here](https://stat.ethz.ch/pipermail/r-help/2010-December/264045.html)
  * an example using `gsubfn` to calculate molecular weights given the chemical formula: [here](http://stackoverflow.com/questions/7012455/sub-handling-of-backreferences/7013992#7013992)

Here are some further examples.
```
library(gsubfn)
library(help = gsubfn) # list help files available
?gsubfn # show a specific help file
vignette("gsubfn") # show vignette 
demo(package = "gsubfn") # list demos available
demo("gsubfn-si") # run a specific demo

# gsubfn - for each number in input string reduce the number of digits after decimal to 3
# problem comes from: http://yihui.name/en/2009/08/formatting-decimals-in-texts-with-r/

x <- "CC = 16.5547557654 + 0.0173022117998*PP + 0.216234040485 * PP(-1) + 0.810182697599 * (WP + WG)"
gsubfn("[0-9]+[.][0-9]+", ~ formatC(as.numeric(x), digit = 3, format = "f"), x)


# strapply - return words from a string
# output is: c('the', 'big', 'brown', 'cat')

strapply('the big brown cat', '\\w+', c)[[1]]

# gsubfn with function represented by formula - increment each number
# output is: '35 abc45g7'

gsubfn('[0-9]+', ~ as.numeric(x) + 1, '34 abc44g6')

# an example with a longer function.  Escapes each punctuation character
# in s with \\ and each non-punctuation character with [...]

s <- '(ab)'
gsubfn('.', 
  ~ if (any(grep("[[:punct:]]", x))) paste0('\\', x) else paste0('[', x, ']'), 
  s)

# gsubfn and proto - replace each number with cumulative sum
# The statement incrementing sum could alternately be written: sum <<- sum + as.numeric(x)
# output is: '34 abc78g84'

p <- proto(pre = function(this) this$sum <- 0,
 fun = function(this, x) this$sum <- this$sum + as.numeric(x)
)
gsubfn('[0-9]+', p, '34 abc44g6')
p$sum

# fn$ - specify aggregate function using a formula
# CO2 is a built in data set in R
fn$aggregate(CO2[4:5], CO2[3], ~mean(range(x)))

# convert date ending in 2 digit year to 4 digit year using cutoff of 10 on year
library(gsubfn)
gsubfn('..$', ~ as.numeric(x) + 100*(as.numeric(x) < 10) + 1900, '1-Mar-50')

# running mean of length 3 of Sepal.Length for each Species
attach(iris)
fn$tapply(Sepal.Length, Species, ~ diff(c(NA,NA,0,cumsum(x)),3)/3, simplify=c)
```
Illustrates use of perl regular expressions
```
# returns text between pat1 and pat2
# In this example, output is: c('name1', 'name2')
# Note that (?U) turns on ungreedy perl-style matching.

a <- 'something2 ....pat1 name1 pat2 something2....pat1 name2 pat2....'
strapply(a, '(?U)pat1 (.*) pat2', perl = TRUE)[[1]]
```
Replace tabs with the appropriate number of spaces (assuming tabs every 8 spaces):
```
# detabbing - slightly modified from original by Greg Snow
# https://stat.ethz.ch/pipermail/r-help/2008-December/182086.html
tmp <- strsplit('one\ttwo\nthree\tfour\n12345678\t910\na\tbc\tdef\tghi\n','\n')[[1]]
out <- gsubfn('([^\t]*)\t', ~ sprintf("%s%*s", x, 8-nchar(x)%%8, " "), tmp)
for(o in out) cat(o, "\n")
```
Mixed sort
```
# mixed ord and mixed sort - Input, s, is a character vector.  Treating each substring of numerics
#  and each substring of non-numerics as a key field, we regard these as records 
#  of key fields which are sorted and returned.
# Internals: L is a list of character vectors whose ith component is the split up 
#  numerics and non-numerics in input s[i].  e.g. L[[3]] is c("x", "02", "b") in
#  example below since s[3] is "x02b".   We arrange this into matrix, L2, so that components
#  of L correspond to rows in L2 replacing NAs with "" to give L3. Finally convert 
#  it to data frame, getting the ordering, ord, and apply that to original 
#  character vector, s.
# from: https://stat.ethz.ch/pipermail/r-help/2008-December/183209.html

mixord <- function(s) {
   L <- strapply(s, "([0-9]+)|([^0-9]+)", ~ if (nchar(x)) sprintf("%99s", x) else y)
   L2 <- t(do.call(cbind, lapply(L, ts)))
   L3 <- replace(L2, is.na(L2), "")
   do.call(order, as.data.frame(L3, stringsAsFactors = FALSE))
}
mixsort <- function(s) s[mixord(s)]

s <- c("x1b", "x1a", "x02b", "x02a", "x02", "y1a1", "y10a2", "y10a10", 
"y10a1", "y2", "var10a2", "var2", "y10")
mixsort(s)

# another mixsort example. Note that mixedsort in gtools gives wrong order here
# but our mixsort works.

ss <- c("a.0", "a.1", "a.2", "a.11", "a.12", "a.20")
mixsort(ss)  # "a.0"  "a.1"  "a.2"  "a.11" "a.12" "a.20"

```
Illustrates using a list replacement argument in gsubfn to convert hex to binary:
```
# hex to binary
# from: https://stat.ethz.ch/pipermail/r-help/2009-May/198655.html

> library(gsubfn)
> binary.digits <- 
+ list("0"= "0000", "1"= "0001", "2"= "0010", "3"= "0011",
+      "4"= "0100", "5"= "0101", "6"= "0110", "7"= "0111",
+      "8"= "1000", "9"= "1001", "A"= "1010", "B"= "1011",
+      "C"= "1100", "D"= "1101", "E"= "1110", "F"= "1111")
>
> gsubfn("[0-9A-F]", binary.digits, "0X1.921FB54442D18P+1")
[1] "0000X0001.1001001000011111101101010100010001000010110100011000P+0001"

```

Illustration of the use of `&` to refer to entire match.  This one suffixes repeated non-vowels with the length of the run.

```
# this example requires at least gsubfn 0.5-3 and 
# an R installation with tcltk. (Most R installations include tcltk.)

qwe <- c("This is a tuff example", "He typed: LLLOLL",
       "And then she was like, 'LOLLLLL'")

gsubfn("([^aeiou])\\1+", ~ paste0(`&`, "<n=", nchar(`&`), ">"), qwe)

# this version does not use `&` so it works even without tcltk
# (It also requires gsubfn 0.3-5 or later.)

gsubfn("(([^aeiou])\\2+)", ~ paste0(..1, "<n=", nchar(..1), ">"), qwe)


```

Implementing Different String Interpolation Styles.  The first is the default style but by merely specifying a different pattern one can alter it.  Each of these give the same output:
```
gsubfn(,, "pi=$pi pi=`2*pi`")

pat <- "[$]([[:alpha:]][[:alnum:].]*)|[$][{]([^}]*)[}]"
gsubfn(pat,, "pi=$pi 2pi=${2*pi}")

pat2 <- "@([^@]*)@"
gsubfn(pat2,, "pi=@pi@ 2pi=@2*pi@")

pat2 <- "%([^%]*)%"
gsubfn(pat2,, "pi=%pi% 2pi=%2*pi%")

pat4 <- "{{(.*?)}}"
gsubfn(pat4,, "pi={{pi}} 2pi={{2*pi}}")
```

# REGULAR EXPRESSION LINKS #

In Programming Languages

  * [in awk](http://awk.info/?fastre)
  * [in Perl](http://www.pcre.org/) ([Wikipedia](http://en.wikipedia.org/wiki/Perl_Compatible_Regular_Expressions))
  * [in PHP - 15 examples](http://www.catswhocode.com/blog/15-php-regular-expressions-for-web-developers)
  * [in R](http://stat.ethz.ch/R-manual/R-patched/library/base/html/regex.html)
  * [in tcl](http://www.tcl.tk/man/tcl8.6/TclCmd/re_syntax.htm)  ([word boundaries in tcl](https://groups.google.com/group/comp.lang.tcl/msg/bd9a5a1482baefb8?))

Examples

  * [email addresses](http://fightingforalostcause.net/misc/2006/compare-email-regex.php) [(more email addresses)](http://emailregex.com/)
  * [match HTML tags](http://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags)
  * [Primes](http://zmievski.org/2010/08/the-prime-that-wasnt?)
  * [URLs](http://www.regexguru.com/2008/11/detecting-urls-in-a-block-of-text/) [(more URLs)](http://mathiasbynens.be/demo/url-regex)
  * [Adding commas separator to an integer](http://www.reddit.com/r/tinycode/comments/wg8nl/adding_a_thousands_separator_to_any_integer_in/)
  * [More Examples](http://www.mkyong.com/regular-expressions/10-java-regular-expression-examples-you-should-know/)
  * [email addresses](http://code.iamcal.com/php/rfc822/full_regexp.txt)
  * [prime numbers](http://www.noulakaz.net/weblog/2007/03/18/a-regular-expression-to-check-for-prime-numbers/)
  * [Is there a regular expression to detect a valid regular expressions](http://stackoverflow.com/questions/172303/is-there-a-regular-expression-to-detect-a-valid-regular-expression)

Processing Tools

  * [analyzer](http://regexp.resource.googlepages.com/analyzer.html) - given a javascript or perl regex it generates a word description of it
  * [debuggex](http://www.debuggex.com) - javascript, python & perl regex tester and visualizer; stackoverflow interface
  * [Nregex](http://www.nregex.com) - test and replace regex matches; C# code generator
  * [regex101](http://regex101.com) - can format, test, debug & generate javascript, python, php code
  * [regexper](http://www.regexper.com/) ([source](https://github.com/javallone/regexper-static)) - javascript regex tester
  * [regex explainer](http://rick.measham.id.au/paste/explain.pl) - produces text based explanation of a regex
  * [regulex](http://jex.im/regulex/) - javascript regex tester & visualizer
  * [regviz](http://regviz.org) - javascript regex tester & visualizer
  * [Rubular](http://rubular.com/) - ruby regular expression tester
  * [txt2re](http://txt2re.com/index.php3) - given example text generate Perl, PHP, Python, Java, Javascript, ColdFusion, C, C++, Ruby, VB, VBScript, J#.net, C#.net, C++.net or VB.net code to match it.

Other

  * [12 regex resources](http://www.webresourcesdepot.com/12-resources-for-mastering-regular-expressions/)
  * http://www.regular-expressions.info
  * [regexone interactive tutorial](http://regexone.com/)
  * [Learm Regular expressions in 55 minutes - tutorial](http://qntm.org/files/re/re.html)
  * [fast vs. slow regex](http://swtch.com/~rsc/regexp/regexp1.html)
  * [Syntax summary](http://www.greenend.org.uk/rjk/2002/06/regexp.html)
  * [Wikipedia on Regex](http://en.wikipedia.org/wiki/Regular_expression)
  * [Writing a regex parser with Thompson's Algorithm](http://www.codeguru.com/cpp/cpp/cpp_mfc/parsing/article.php/c4093)
  * [Writing a regex parser with recursive descent](http://matt.might.net/articles/parsing-regex-with-recursive-descent/)
  * [books](http://www.google.com/search?tbm=bks&tbo=1&q=regular+expressions&btnG=Search+Books)
  * [Searching gmail with regex](http://www.labnol.org/internet/advanced-gmail-search/21623/)
  * [Problems with backtracking in regular expressions](https://web.archive.org/web/20130525063307/http://tech.blog.cueup.com/regular-expressions-will-stab-you-in-the-back)
  * [Incremental Regular Expressions](http://jkff.info/articles/ire/)
  * [Russ Cox Articles](http://swtch.com/~rsc/regexp/)
  * [The True Power of Regular Expressions](http://nikic.github.io/2012/06/15/The-true-power-of-regular-expressions.html) (shows that regular expressions as implemented in most programming languages can match all context free and some context sensitive grammars - not just regular grammars)
  * [Finite State Machines and Regular Expressions](http://www.gamedev.net/page/resources/_/technical/general-programming/finite-state-machines-and-regular-expressions-r3176)
  * [Regex Crossword](http://regexcrossword.com/)
  * [RegHex](http://rampion.github.io/RegHex/)
  * [Regex Game](https://github.com/Liniarc/regexProgram)
  * [Stackoverflow regex tag](http://stackoverflow.com/questions/tagged/regex) [(tutorial)](http://stackoverflow.com/questions/4736/learning-regular-expressions)