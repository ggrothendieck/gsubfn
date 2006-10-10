strapply <- 
function (X, pattern, FUN = function(x, ...) x, ..., simplify = FALSE, 
    USE.NAMES = FALSE, combine = c) 
{
    FUN <- match.funfn(FUN)
    result <- sapply(X, function(x) {
        v <- NULL
        gsubfn(pattern, function(x, ...) 
		v <<- c(v, combine(FUN(x, ...))), x, ...)
	v
    }, simplify = is.logical(simplify) && simplify, USE.NAMES = USE.NAMES)
    if (is.logical(simplify)) 
        result
    else do.call(simplify, result)
}
