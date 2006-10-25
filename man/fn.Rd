\name{fn}
\alias{fn}
\alias{$.fn}
\title{ Transform formula arguments to functions. }
\description{
  When used in the form \code{fn\$somefunction(...arguments...)}
  it converts formulas among the arguments of somefunction to
  functions using \code{\link{as.function.formula}}.  It uses a heuristic
  to decide which formulas to convert.  If any of the following
  are true then that argument is converted from a formula to a
  function: (1) there is only one formula among the arguments,
  (2) the name of the formula argument is \code{FUN} or
  (3) the formula argument is not the first argument in the
  argument list.

  It also removes any \code{simplify} argument whose value is
  not logical and after processing it in the same way just
  discussed in order to interpret it as a function it passes
  the output of the command through \code{do.call(simplify, output)}.

  It also performs quasi-perl style string interpolation on any 
  character string arguments that begin with \code{\\1} removing
  the \code{\\1} character.  A dollar sign followed by a variable
  name or \code{R} code within backticks are both evaluated.
}
\usage{
"$.fn"(x, FUN) 
}
\arguments{
  \item{x}{ \code{fn}.}
  \item{FUN}{Name of a function.}
}
\value{
  Returns a function.
}
\seealso{\link{as.function.formula}}
\examples{

   # use of formula to specify a function.
   # Note that LETTERS, letters and pi are automatically excluded from args
   fn$lapply(list(1:4, 1:3), ~ LETTERS[x])
   fn$sapply(1:3, ~ sin((n-1) * pi/180))

   # use of simplify = rbind instead of do.call(rbind, by(...)).
   # args to anonymous function are automatically determined.
   fn$by(BOD, 1:nrow(BOD), ~ c(mn = min(x), mx = max(x)), simplify = rbind)

   # calculate lm coefs of uptake vs conc for each Plant
   fn$by(CO2, CO2$Plant, d ~ coef(lm(uptake ~ conc, d)), simplify = rbind)

   # mid range of conc and uptake by Plant
   fn$aggregate(CO2[,4:5], CO2[1], ~ mean(range(x)))

   # string interpolation
   j <- fn$cat("pi = $pi, exp = `exp(1)`\n")

\dontrun{

   # same but use cast/melt from reshape package
   library(reshape)
   fn$cast(Plant ~ variable, data = melt(CO2, id = 1:3), ~~ mean(range(x)))

   # same
   # uncomment when new version of doBy comes out (expected shortly)
   # library(doBy)
   # fn$summaryBy(.~Plant,CO2[-(2:3)],FUN= ~~mean(range(x)), pref='midrange')
}

   # generalized matrix product
   # can replace sum(x*y) with any other inner product of interest
   # this example just performs matrix multiplication of a times b
   a <- matrix(4:1, 2)
   b <- matrix(1:4, 2)
   fn$apply(b, 2, x ~ fn$apply(a, 1, y ~ sum(x*y)))

   # integration
   fn$integrate(~1/((x+1)*sqrt(x)), lower = 0, upper = Inf)

   # optimization
   fn$optimize(~ x^2, c(-1,1))

   # using fn with S4 definitions
   setClass('ooc', representation(a = 'numeric'))
   fn$setGeneric('incr', x + value ~ standardGeneric('incr'))
   fn$setMethod('incr', 'ooc', x + value ~ {x@a <- x@a+value; x})
   oo <- new('ooc',a=1)
   oo <- incr(oo,1)
   oo

\dontrun{

   # plot quantile regression fits for various values of tau
   library(quantreg)
   data(engel)
   plot(engel$x, engel$y, xlab = 'income', ylab = 'food expenditure')
   junk <- fn$lapply(1:9/10, tau ~ abline(coef(rq(y ~ x, tau, engel))))

   # rolling mid-range
   library(zoo)
   fn$rollapply(LakeHuron, 12, ~ mean(range(x)))

   library(lattice)
   fn$xyplot(uptake ~ conc | Plant, CO2,
      panel = ... ~ { panel.xyplot(...); panel.text(200, 40, lab = 'X') })

   library(boot)
   set.seed(1)
   fn$boot(rivers, ~ median(x, d), R = 2000)
}

   x <- 0:50/50
   matplot(x, fn$outer(x, 1:8, ~ sin(x * k*pi)), type = 'blobcsSh')


}
\keyword{ programming }
