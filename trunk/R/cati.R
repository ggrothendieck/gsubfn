
# same as cat except it replaces $x (where x starts with letter and may contain
# letters, numbers and dots) or `x` (where x is any expression not containing 
# backticks) with x evaluated and appends \n at end
# The end= argument can be used to append additional characters.
# e.g. cati("letters = $letters, pi = $pi\n")
cati <- 
function (..., file = "", sep = " ", fill = FALSE, labels = NULL, 
    append = FALSE, env = parent.frame(), 
    pattern = "[$]([[:alpha:]][[:alnum:].]*)|`([^`]+)`", 
    backref = nchar(base::gsub("[^(]","",pattern)), 
    end = "") 
{
	# added end= argument to cat
	cat.end <-
	function (..., file = "", sep = " ", fill = FALSE, labels = NULL, 
	    append = FALSE, end = "") 
	{
	    if (is.character(file)) 
		if (file == "") 
		    file <- stdout()
		else if (substring(file, 1, 1) == "|") {
		    file <- pipe(substring(file, 2), "w")
		    on.exit(close(file))
		}
		else {
		    file <- file(file, ifelse(append, "a", "w"))
		    on.exit(close(file))
		}
	    args <- list(...)
	    n <- length(args)
	    args[[n]] <- paste(args[[n]], end, sep = "")
	    .Internal(cat(args, file, sep, fill, labels, append))
	}

    force(env)
    args <- lapply(list(...), function(x) 
               gsubfn(pattern, , as.character(x), env = env))
    args <- c(args, file = file, sep = sep, fill = fill, labels = labels,
	append = append, end = end)
    do.call("cat.end", args)
}

cati0 <- function(..., sep = "") cati(..., sep = sep)
cat0 <- function(..., sep = "") cat(..., sep = sep)
paste0 <- function(..., sep = "") paste(..., sep = sep)
