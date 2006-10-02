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

