
aggregatefn <- function(x, by, FUN, ..., simplify = FALSE) {
	FUN <- match.funfn(FUN)
	if (!is.logical(simplify)) 
	   do.call(simplify, aggregate(x, by, FUN, ...))
	else by(x, by, FUN, ...)
}

applyfn <- function(..., simplify = FALSE) {
	match.fun <- match.funfn
	environment(apply) <- environment()
	apply(...)
}

byfn <- function(data, indices, FUN, ..., simplify = FALSE) {
	FUN <- match.funfn(FUN)
	if (is.logical(simplify) && simplify) simplify <- rbind
	if (!is.logical(simplify)) 
  	   do.call(simplify, by(data, indices, FUN, ...))
	else by(data, indices, FUN, ...)
}

eapplyfn <- function(..., simplify = FALSE) {
	match.fun <- match.funfn
	environment(eapply) <- environment()
	if (is.logical(simplify) && simplify) simplify <- rbind
	if (is.logical(simplify)) eapply(...)
	else do.call(simplify, eapply(...))
}

lapplyfn <- function(..., simplify = FALSE) {
	match.fun <- match.funfn
	environment(lapply) <- environment()
	if (is.logical(simplify) && simplify) simplify <- rbind
	if (is.logical(simplify)) lapply(...)
	else do.call(simplify, lapply(...))
}


mapplyfn <- function(..., SIMPLIFY = TRUE) {
	match.fun <- match.funfn
	environment(mapply) <- environment()
	if (!is.logical(SIMPLIFY)) 
		do.call(SIMPLIFY, mapply(..., SIMPLIFY = FALSE))
	else mapply(..., SIMPLIFY = SIMPLIFY)
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

Vectorizefn <- function(..., SIMPLIFY = TRUE) {
	match.fun <- match.funfn
	environment(Vectorize) <- environment()
	if (!is.logical(SIMPLIFY)) 
		do.call(SIMPLIFY, Vectorize(..., SIMPLIFY = FALSE))
	else Vectorize(..., SIMPLIFY = SIMPLIFY)
}

