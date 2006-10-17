
strapply <- 
function (X, pattern, FUN = function(x, ...) x, ...,
    simplify = FALSE, USE.NAMES = FALSE, combine = c) 
{
    FUN <- match.funfn(FUN)
    ff <- function(x) {
        v <- NULL
	first <- TRUE
        gsubfn(pattern, function(x, ...) { 
		# print(x); print(FUN(x, ...))
		v <<- if (first) combine(FUN(x, ...))
		else c(v, combine(FUN(x, ...))) 
		first <<- FALSE
	}, x, ...)
	v
    }
    result <- sapply(X, ff,
	simplify = is.logical(simplify) && simplify, USE.NAMES = USE.NAMES)
    if (is.logical(simplify)) 
        result
    else do.call(match.funfn(simplify), result)
}


