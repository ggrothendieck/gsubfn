as.function.formula <- function(x, ...) {
	vars <- all.vars(x)
	f <- function() {}
	formals(f) <- structure(rep("", length(vars)), .Names = vars)
	body(f) <- x[[2]]
	environment(f) <- environment(x)
	f
}
