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

   here <- environment()

   if (missing(replacement)) here$replacement <- function(...) 
	eval(parse(text = paste(..., sep = "")), env) 

   if (is.character(replacement)) 
	return(base::gsub(pattern, replacement, x, ...))

   if (is.list(replacement)) {
			values.replacement <- replacement
			names.replacement <- names(replacement)
			here$replacement <- function(...) {
				idx <- match(..1, names.replacement, 
					nomatch = match("", names.replacement, nomatch = 0))
				if (idx > 0) values.replacement[[idx]]
				else ..1
			}
    }
   # if (inherits(replacement, "formula")) replacement <- as.function(replacement)
   if (missing(pattern)) pattern <- "[$]([[:alpha:]][[:alnum:].]*)|`([^`]+)`"
   # i is 1 if the entire match is passed and 2 otherwise.
   # j is 1 plus the number of backreferences
   if (missing(backref) || is.null(backref)) {
    noparen <- base::gsub("\\\\.", "", pattern)
    noparen <- base::gsub("\\[[^\\]]*\\]", "", noparen)
	j <- nchar(base::gsub("[^(]","", noparen))+1
	i <- min(2, j)
   } else {
	i <- as.numeric(backref < 0) + 1
	j <- abs(backref)+1
   }

   e <- NULL
   if (!inherits(replacement, "formula") && !is.function(replacement)) {
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
	here$replacement <- e$replacement
   }
   here$replacement <- match.funfn(replacement)
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
	  # replace match with \1\2 \\1 \2 \\2 \2 ... \1
	  # replace each backref in regexp with 
	  # \1\2 followed by backrefs separated by \2 all followed by \1
	  # Using that create a string \1\2 first backref \2 second ... \1
	  # and perform replacement
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

ostrapply <- 
function (X, pattern, FUN = function(x, ...) x, ...,
    simplify = FALSE, USE.NAMES = FALSE, combine = c) {
	here <- environment()
	combine <- match.funfn(combine)
	if (is.character(FUN)) {
		FUN.orig <- FUN
		FUN <- function(...) FUN.orig
	} else if (is.list(FUN)) {
		values.replacement <- FUN
		names.replacement <- names(FUN)
		here$FUN <- function(...) {
			idx <- match(..1, names.replacement, 
				nomatch = match("", names.replacement, nomatch = 0))
			if (idx > 0) values.replacement[[idx]] else ..1
		}
    }
   
    p <- if (is.proto(FUN)) {
		FUN$X <- X
		FUN$pattern <- pattern
		FUN$simplify <- simplify
		FUN$USE.NAMES <- USE.NAMES
		FUN$combine <- combine
		proto(
			pre = function(this) { 
				this$first <- TRUE
				this$v <- NULL
				if (!is.null(FUN[["pre"]])) FUN$pre()
			},
			fun = function(this, ...) {
				FUN$count <- this$count
				this$v <- if (this$first) combine(FUN$fun(...))
				else c(this$v, combine(FUN$fun(...)))
				this$first <- FALSE
			},
			post = function(this) {
				# cat("A:", first, "\n")
				if (this$first) this$v <- NULL
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
				this$v <- if (this$first) combine(FUN(...))
				else c(this$v, combine(FUN(...)))
				this$first <- FALSE
			},
			post = function(this) { 
				# cat("B:", first, "\n")
				if (this$first) this$v <- NULL  
			}
		)
        }
    ff <- function(x) { gsubfn(pattern, p, x, ...); p$v }
    result <- sapply(X, ff, 
	simplify = is.logical(simplify) && simplify, USE.NAMES = USE.NAMES)
    if (is.logical(simplify)) result else {
		do.call(match.funfn(simplify), result)
	}
}

strapply <-
function (X, pattern, FUN = function(x, ...) x, backref = NULL, ...,
	ignore.case = FALSE, perl = FALSE, engine = c("tcl", "R"), 
	simplify = FALSE, USE.NAMES = FALSE, combine = c) {
				engine <- match.arg(engine)
				if (engine == "R" || is.proto(FUN) || perl) return(ostrapply(X = X, 
						pattern = pattern, FUN = FUN, backref = backref, 
						..., perl = perl, simplify = simplify, USE.NAMES = USE.NAMES, 
						combine = combine))
                if (is.proto(FUN)) {
                        # TODO
                } else if (is.character(FUN)) {
                        FUN.orig <- FUN
                        FUN <- function(...) FUN.orig
                } else if (is.list(FUN)) {
                        values.replacement <- FUN
                        names.replacement <- names(FUN)
                        FUN <- function(...) {
                                idx <- match(..1, names.replacement, 
                                        nomatch = match("", names.replacement, nomatch = 0))
                                if (idx > 0) values.replacement[[idx]] else ..1
                        }
                } else {
                        FUN <- match.funfn(FUN)
                }
				ff <- function(x) {
					s <- strapply1(x, pattern, backref, ignore.case)
					L <- lapply(seq_len(ncol(s)), function(j) {
						combine(do.call(FUN, as.list(s[, j]))) })
						# combine(do.call(FUN, list(s[, j]))) })
					do.call("c", L)
				}
                result <- sapply(X, ff,
                        simplify = is.logical(simplify) && simplify, 
                        USE.NAMES = USE.NAMES)
                if (is.logical(simplify)) result
                else do.call(match.funfn(simplify), result)

}

strapply1 <- function(x, e, backref, ignore.case = FALSE) {
        tcl("set", "e", e)
        tcl("set", "x", x)
        .Tcl('set about [regexp -about -- $e]')
		about <- as.numeric(tclvalue(.Tcl("lindex $about 0"))) + 1
		s <- if (ignore.case) 'set r [regexp -all -inline -nocase -- $e $x]'
		else 'set r [regexp -all -inline -- $e $x]'
        # .Tcl('set r [regexp -all -inline -- $e $x]')
        .Tcl(s)
        n <- as.numeric(tclvalue(.Tcl("llength $r")))
        out <- sapply(seq(0, length = n), 
                function(i) tclvalue(.Tcl(paste("lindex $r", i))))
		out <- matrix(out, about)
		if (is.null(backref)) {
			if (about > 1) out[-1,, drop = FALSE] 
			else out
		} else {
			mn <- 1 + backref > 0
			mx <- min(abs(backref) + 1, about)
			out[seq(mn, mx),, drop = FALSE]
		}
}

