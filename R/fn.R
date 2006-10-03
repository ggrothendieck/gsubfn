
as.function.formula <- function(x, ...) {
	vars <- all.vars(x[[2]])
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
match.funfn.default <- base::match.fun
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
		idx <- match("simplify", tolower(nm), nomatch = 0)
		simplify <- NULL
		if (idx > 0) {
			if (!is.logical(args[[idx]])) {
				simplify <- args[[idx]]
				args <- args[-idx]
				nm <- nm[-idx]
			}
		}
		is.fo <- sapply(args, function(x) is(x, "formula"))
		num.fo <- sum(is.fo)

		is.funfo <- is.fo & (num.fo == 1 | seq(along = args) > 1 | 
			nm == "FUN")
		mcList <- as.list(mc)[-1]
		if (idx > 0) mcList <- mcList[-idx]
	
		for(i in seq(along = args)) {
		   if (is.fo[i] && (num.fo == 1 || i > 1 || nm[[i]] == "FUN"))
		         mcList[[i]] <- as.function(args[[i]])
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

