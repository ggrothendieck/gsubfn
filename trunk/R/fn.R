eval.with.vis <- function (expr) {
     expr <- substitute(expr)
     pf <- parent.frame()
     tmp <- .Internal(eval.with.vis(expr, pf,
         baseenv()))
     tmp
}

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

		# is.fo is a logical vector indicating whether
		#    each list element is or is not a formula
		# is.fo2 is a logical vector indicating whether each
		#    list element has or does not have a ~~ (double ~)

		is.fo <- sapply(mcListE, function(x) is(x, "formula"))
		any.fo <- any(is.fo)
		is.fo2 <- sapply(mcListE, function(x) is(x, "formula") &&
			length(x[[length(x)]]) > 1 &&
			identical(x[[length(x)]][[1]], as.name("~")))
		# change ~~ to ~
		any.fo2 <- any(is.fo2)
		if (any.fo2)
		   for(i in seq(along = mcListE))
			if (is.fo2[i]) {
			   len <- length(mcListE[[i]])
			   mcListE[[i]][[len]] <- mcListE[[i]][[len]][[2]]
			   mcListE[[i]] <- as.function(mcListE[[i]])
			} 
					
		is.char <- sapply(mcListE, is.character)
		any.char <- any(is.char)
		is.chara <- sapply(mcListE, function(x) 
			is.character(x) && substring(x, 1, 1) == "\1")
		# remove leading \1 on character strings
		any.chara <- any(is.chara)
		if (any.chara)
		   for(i in seq(along = mcListE))
		      if (is.chara[i])
			mcListE[[i]] <- gsubfn(x = substring(mcListE[[i]], 2))

		# if no ~~ formulas and no \1 strings use default strategy
		# of converting all formulas to functions and if no formulas
		# performing perl-style interpolation on all strings
		if (!any.fo2 && !any.chara) {
		   if (any.fo) {
		      for(i in seq(along = mcListE))
		         if (is.fo[i])
			    mcListE[[i]] <- as.function(mcListE[[i]])
		   } else {
		      if (any.char)
		         for(i in seq(along = mcListE))
		            if (is.char[i])
			       mcListE[[i]] <- gsubfn(x = mcListE[[i]])
		   }
		}
			
		# out <- do.call(FUN, args)
		# thanks Duncan for eval.with.vis !!!
		out <- eval.with.vis(do.call(FUN, mcListE, env=parent.frame()))
		vis <- out$visible
		out <- out $value
		if (!is.null(simplify)) {
			if(!is.list(out)) out <- list(out) 
			out <- eval.with.vis(do.call(simplify, out))
			vis <- out$visible
			out <- out$value
		}
		if (vis) out else invisible(out)
	}
}
		
# test
# fn$list(x ~ 2*x)
# fn$mapply(~ x + y, 1:10, 21:30)

cat0 <- function(..., sep = "") cat(..., sep = sep)
paste0 <- function(..., sep = "") paste(..., sep = sep)




