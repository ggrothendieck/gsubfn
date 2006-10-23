# supports function as replacement argument.  Matched string is passed to
# function as arg1, with subsequent args being the backreferences.  
# Backref is number of backrefs that are passed to function and is normally 
# left at default value although it can be set lower for improved efficiency, 
# e.g. backref = 0 if no backreferences are to be passed.
#
# e.g. gsubfn("[[:digit:]]+", function(x) as.numeric(x)+1, "(10 20)(100 30)") 
#   adds 1 to each number in third arg
#
# e.g. f <- function(x,y,z) as.numeric(y)+as.numeric(z),
#      gsubfn("([0-9]+):([0-9]+)", f, "abc 10:20 def 30:40 50")
#   replaces pairs m:n with their sum
#
# e.g. gsubfn( , , "pi = $pi, 2pi = `2*pi`") 
#
# e.g. v <- c(); f <- function(x) v <<- append(v,as.numeric(x))
#      gsubfn("[0-9]+", f, "12;34:56,89,,12")
#   extracts numbers from string and places them into vector v
#
# e.g. gsubfn("\\B.", tolower, "I LIKE A BANANA SPLIT")
#   makes all letters except first in word lower case
#
gsubfn <- function(pattern, replacement, x, backref, USE.NAMES = FALSE, 
  env = parent.frame(), ...) 
{
   if (missing(replacement)) replacement <- function(x,b1,b2) 
	eval(parse(text = paste(b1,b2,sep="")), env) 
   if (is.character(replacement)) 
	return(base::gsub(pattern, replacement, x, ...))
   # if (inherits(replacement, "formula")) replacement <- as.function(replacement)
   if (missing(pattern)) pattern <- "[$]([[:alpha:]][[:alnum:].]*)|`([^`]+)`"
   if (missing(backref)) {
        i <- 1
	j <- nchar(base::gsub("[^(]","",pattern))+1
   } else {
	i <- as.numeric(backref < 0) + 1
	j <- abs(backref)+1
   }

   e <- NULL
   if (!is(replacement, "formula") && !is.function(replacement)) {
	e <- replacement
	e$pattern <- pattern
	e$x <- x
	e$backref <- if (!missing(backref)) backref
	e$USE.NAMES <- USE.NAMES
	e$env <- env
	dots <- list(...)
	if (!is.null(names(dots))) {
		nam <- names(dots)
		for(n in nam[nam != ""]) assign(n, dots[[n]], e)
	}
	e$replacement <- function(this, ...) {
		this$count <- this$count + 1
		this$match <- c(...)
		this$fun(...)
	}
	replacement <- e$replacement
   }
   replacement <- match.funfn(replacement)
   stopifnot(is.character(pattern), is.character(x), is.function(replacement))
   force(env)
   gsub.function <- function(x) {
      # x <- base::gsub('"', '\\\\"', x)
      # x <- chartr('"', '\b', x)
      # pattern <- chartr('"', '\b', pattern)
      pattern <- paste("(", pattern, ")", sep = "")
      if (!is.null(e)) {
          e$count <- 0
          if ("pre" %in% ls(e)) e$pre()
      }
      repl <- function(i,j) {  
	      rs <- paste('\\', seq(i,j), collapse = "\2", sep = "") 
	      rs <- paste('\1\2', rs, '\1', sep = "")
              # if backref= is too large, reduce by 1 and try again
	      tryCatch(base::gsub(pattern, rs, x, ...),
			error = function(x) if (j > i) repl(i,j-1) else stop(x))
      }
      z <- repl(i,j)
      z <- strsplit(z, "\1")[[1]]
      f <- function(s) {
	if (nchar(s) > 0 && substring(s,1,1) == "\2") {
	    s <- sub("\2$", "\2\2", s)
	    L <- as.list(strsplit(s, "\2")[[1]][-1])
            # if (!is.null(e)) L <- c(list(e), L)
	    do.call(replacement, L)
        } else s
      }
      z <- paste(sapply(z, f), collapse = "")
      if (!is.null(e) && "post" %in% ls(e)) e$post()
      z
      # gsub('\b', '\\\\"', z)
   }
   sapply(x, gsub.function, USE.NAMES = USE.NAMES)
}

strapply <- 
function (X, pattern, FUN = function(x, ...) x, ...,
    simplify = FALSE, USE.NAMES = FALSE, combine = c) 
{
    p <- if (is.proto(FUN)) {
		FUN$X <- X
		FUN$pattern <- pattern
		FUN$simplify <- simplify
		FUN$USE.NAMES <- USE.NAMES
		FUN$combine <- combine
		proto(
			pre = function(this) { 
				this$first <- TRUE
				v <- NULL
				if (!is.null(FUN[["pre"]])) FUN$pre()
			},
			fun = function(this, ...) {
				FUN$count <- count
				this$v <- if (first) combine(FUN$fun(...))
				else c(v, combine(FUN$fun(...)))
				first <<- FALSE
			},
			post = function(this) {
				# cat("A:", first, "\n")
				if (first) this$v <- NULL
				if (!is.null(FUN[["post"]])) FUN$post()
			},
		    ) 
	} else {
		FUN <- match.funfn(FUN)
		proto(
			pre = function(this) { 
				this$first <- TRUE
				this$v <- NULL 
			},
			fun = function(this, ...) {
				this$v <- if (first) combine(FUN(...))
				else c(v, combine(FUN(...)))
				first <<- FALSE
			},
			post = function(this) { 
				# cat("B:", first, "\n")
				if (first) this$v <- NULL  
			}
		)
        }
    ff <- function(x) { gsubfn(pattern, p, x, ...); p$v }
    result <- sapply(X, ff, 
	simplify = is.logical(simplify) && simplify, USE.NAMES = USE.NAMES)
    if (is.logical(simplify)) 
        result
    else do.call(match.funfn(simplify), result)
}





