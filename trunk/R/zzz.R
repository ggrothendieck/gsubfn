
.onLoad <- function(libname, pkgname) {

	if (.Platform$OS.type == "windows" && 
		interactive() && 
		!isTRUE(getOption("gsubfn.noVignetteMenu")) && 
		file.exists(system.file(package = "Zelig")) &&
		file.exists(system.file("Meta", "vignette.rds", package = pkgname))) {
		  Zelig:::addVigs2WinMenu(pkgname)
	}

	gsubfn.engine <- getOption("gsubfn.engine")
	# if gsubfn.engine was not set to "R" then check if tcltk can be used
    if ( ! identical(gsubfn.engine, "R") ) {
		tcltk.ok <- isTRUE(capabilities()[["tcltk"]]) && 
						requireNamespace("tcltk", quietly = TRUE)
		if ( ! tcltk.ok ) {
			options(gsubfn.engine = "R")
			packageStartupMessage('Could not load tcltk.  Will use slower R code instead.')
		}
	}

}

# .onUnload <- function(libpath) {}
