#!/usr/bin/env Rscript

print_help<-function() {
	cat("This is a make script.\n")
	cat("\n")
	cat("Usage: ./make.R TARGET\n")
	cat("\n")
	cat("Targets:\n")
	cat("  document     generate the documentation (i.e. man paged, manual pdf)\n")
	cat("  check        run the package checks\n")
	cat("  check-win    run the package checks for windows plattforms\n")
	cat("  build        build a distributable source package file\n")
	cat("  build-bin    build a distributable binary package file\n")
	cat("  install      install the package into the R library\n")
	cat("  clean        remove generated files\n")
	cat("  all          run document, check, build, install\n")
	cat("  all-win      run document, check, check-win, build, install\n")
}

make_document<-function() {
	library(devtools)
	document()
	manual_path = "doc"
	if(!dir.exists(manual_path))
		dir.create(manual_path)
	build_manual(path=manual_path)
	cat("Manual is created in", manual_path, "\n")
}
make_check<-function(make_document=TRUE, windows=FALSE) {
	library(devtools)
	if(make_document)
		make_document()
	if(windows) {
		#check_win_devel()   # Check package on the development version of R.
		check_win_release()  # Check package on the release version of R.
	} else {
		check()
	}
}
make_build<-function(make_document=TRUE, binary=FALSE) {
	library(devtools)
	if(make_document)
		make_document()
	build(binary=binary)
}
make_install<-function() {
	library(devtools)
	install()
}
make_clean<-function() {
	if(dir.exists("man"))
		system("rm -r man")
	if(dir.exists("doc"))
		system("rm -r doc")
}
make_all<-function(windows=FALSE) {
	make_document()
	make_check(make_document=FALSE, windows=FALSE)
	if(windows)
		make_check(make_document=FALSE, windows=TRUE)
	make_build(make_document=FALSE)
	make_install()
}

main<-function() {
	args = commandArgs(trailingOnly=TRUE)

	if(length(args) == 0 || args[1] %in% c('help', '-h', '--help')) {
		print_help()
	} else if(args[1] == "document") {
		make_document()
	} else if(args[1] == "check") {
		make_check()
	} else if(args[1] == "check-win") {
		make_check(windows=TRUE)
	} else if(args[1] == "build") {
		make_build()
	} else if(args[1] == "build-bin") {
		make_build(binary=TRUE)
	} else if(args[1] == "install") {
		make_install()
	} else if(args[1] == "clean") {
		make_clean()
	} else if(args[1] == "all") {
		make_all()
	} else if(args[1] == "all-win") {
		make_all(windows=TRUE)
	} else {
		cat("Unsupported argument\n")
		print_help()
	}
	quit(save="no")
}

main()
