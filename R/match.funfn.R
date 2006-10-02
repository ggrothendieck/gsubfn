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


match.funfn <- function(x, ...) UseMethod("match.fun")
match.funfn.default <- base::match.fun
match.funfn.formula <- as.function.formula

applyfn <- function(..., simplify = FALSE) {
	match.fun <- match.funfn
	environment(apply) <- environment()
	apply(...)
}

lapplyfn <- function(...) {
	match.fun <- match.funfn
	environment(lapply) <- environment()
	lapply(...)
}

mapplyfn <- function(..., SIMPLIFY = TRUE) {
	match.fun <- match.funfn
	environment(mapply) <- environment()
	if (!is.logical(SIMPLIFY)) 
		do.call(SIMPLIFY, mapply(..., SIMPLIFY = FALSE))
	else mapply(..., SIMPLIFY = SIMPLIFY)
}


Vectorizefn <- function(..., SIMPLIFY = TRUE) {
	match.fun <- match.funfn
	environment(Vectorize) <- environment()
	if (!is.logical(SIMPLIFY)) 
		do.call(SIMPLIFY, Vectorize(..., SIMPLIFY = FALSE))
	else Vectorize(..., SIMPLIFY = SIMPLIFY)
}


sapplyfn <- function(..., simplify = TRUE) {
	match.fun <- match.funfn
	environment(sapply) <- environment()
	if (!is.logical(simplify)) 
		do.call(simplify, sapply(..., simplify = FALSE))
	else sapply(..., simplify = simplify)
}

tapplyfn <- function(..., simplify = TRUE) {
	match.fun <- match.funfn
	environment(tapply) <- environment()
	if (!is.logical(simplify)) 
		do.call(simplify, tapply(..., simplify = FALSE))
	else tapply(..., simplify = simplify)
}

byfn <- function(data, indices, FUN, ..., simplify = FALSE) {
	FUN <- match.funfn(FUN)
	if (is.logical(simplify) && simplify) simplify <- rbind
	if (!is.logical(simplify)) 
  	   do.call(simplify, by(data, indices, FUN, ...))
	else by(data, indices, FUN, ...)
}

aggregatefn <- function(x, by, FUN, ..., simplify = FALSE) {
	FUN <- match.funfn(FUN)
	if (!is.logical(simplify)) 
	   do.call(simplify, aggregate(x, by, FUN, ...))
	else by(x, by, FUN, ...)
}
