
.onLoad <- function(libname, pkgname) {
	gsubfn.engine <- getOption("gsubfn.engine")
	# if gsubfn.engine was not set to "R" then check if tcltk can be used
    if (!identical(gsubfn.engine, "R")) {
		# if we could be sure that capabilities is always TRUE/FALSE then
		# isTRUE could be removed
		tcltk.ok <- isTRUE(capabilities()[["tcltk"]]) && (require)("tcltk")
		if (!tcltk.ok) {
			options(gsubfn.engine = "R")
			warning('Unable to use tcltk package.  Will use slower R code instead. Option gsubfn.engine set to "R".')
		}
	}

}

# .onUnload <- function(libpath) {}
