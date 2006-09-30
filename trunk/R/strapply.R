
#strapply <- function(X, pattern, FUN = function(x, ...) x, ..., 
#   simplify = FALSE, USE.NAMES = FALSE)
#      sapply(X, function(x) {
#          v <- NULL
#	  gsubfn(pattern, function(x, ...) v <<- c(v, FUN(x, ...)), x, ...)
#	  v
#	  }, simplify = simplify, USE.NAMES = USE.NAMES)


strapply <- 
function (X, pattern, FUN = function(x, ...) x, ..., simplify = FALSE, 
    USE.NAMES = FALSE) 
	sapply(X, function(x) {
	    first <- TRUE
	    v <- NULL
	    gsubfn(pattern, function(x, ...)
		v <<- if (first) { first <<- FALSE; FUN(x, ...) } 
		else c(v, FUN(x, ...)),
		x, ...)
	    v
	}, simplify = simplify, USE.NAMES = USE.NAMES)

