
as.function.formula <- function(x, ...) {
	vars <- setdiff(all.vars(x[[2]]), c("letters", "LETTERS", "pi"))
	if (length(vars) == 0) { 
		f <- function() {}
	} else {
		f <- function(x) {}
		formals(f) <- rep(formals(f), length(vars))
		names(formals(f)) <- vars
	}
	body(f) <- x[[length(x)]]
	environment(f) <- environment(x)
	f
}

match.funfn <- function(x, ...) UseMethod("match.funfn")
match.funfn.default <- function(x, ...) base::match.fun(x, ...)
match.funfn.formula <- as.function.formula

fn <- structure(NA, class = "fn")
"$.fn" <- function(x, FUN) {
	env <- parent.frame()
	mf <- match.fun(FUN)

	function(...) {
		args <- list(...)
		mc <- if (is.primitive(mf)) match.call()
		else match.call(mf)
		mc1 <- mc[-1]
		nm <- names(mc1)
		if (is.null(nm)) nm <- rep("", length(args))

		mcList <- as.list(mc1)
		p <- parent.frame()
		mcListE <- lapply(mcList, eval, p)

		# if simplify found set it and remove it from lists
		simplify <- NULL
		idx <- match("simplify", tolower(nm), nomatch = 0)
		if (idx > 0) {
			if (!is.logical(mcListE[[idx]])) {
				simplify <- mcListE[[idx]]
				mcListE <- mcListE[-idx]
				mcList <- mcList[-idx]
				nm <- nm[-idx]
			}
		}

		# arg1.idx is the location of argument 1 in mcList
		# is.fo is a logical vector indicating whether
		# each list element is or is not a formula
		# is.funfo is true for formulas to be translated


		is.fo <- sapply(mcListE, function(x) is(x, "formula"))
		is.char <- sapply(mcListE, function(x) 
			is.character(x) && substring(x, 1, 1) == "\1")
		arg1.idx <- 0
		if (is(args[[1]], "formula"))
		   for(i in seq(along = mcListE))
		      if (is.fo[i] && format(mcList[[i]]) == format(args[[1]]))
		         arg1.idx <- i
		num.fo <- sum(is.fo)
		is.funfo <- is.fo & (num.fo == 1 | 
			seq(along = mcList) != arg1.idx | 
			nm == "FUN")
	
		#for(i in seq(along = args)) {
		#   if (is.fo[i] && (num.fo == 1 || i > 1 || nm[[i]] == "FUN"))
		#         mcList[[i]] <- as.function(args[[i]])
		for(i in seq(along = mcList)) {
		   if (is.funfo[i]) {
		         # mcList[[i]] <- as.function(args[[i]])
			 mcList[[i]] <- as.function(mcListE[[i]])
		   }
		   if (is.char[i]) {
			mcList[[i]] <- gsubfn(x = substring(mcList[[1]], 2))
		   }
		}
		# out <- do.call(FUN, args)
		out <- do.call(FUN, mcList, env = parent.frame())
		if (!is.null(simplify)) {
			if(!is.list(out)) out <- list(out) 
			out <- do.call(simplify, out)
		}
		out
	}
}
		
# test
# fn$list(x ~ 2*x)
# fn$mapply(~ x + y, 1:10, 21:30)

cat0 <- function(..., sep = "") cat(..., sep = sep)
paste0 <- function(..., sep = "") paste(..., sep = sep)


