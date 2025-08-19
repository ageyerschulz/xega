#
# (c) 2021 Andreas Geyer-Schulz
#          Simple Genetic Algorithm in R. V 0.1
#          Layer: Top-level main programs.     
#          Package: xega
#

#' About this version.
#'
#' @param verbose  Boolean. If \code{TRUE} (Default), print 
#'                 package information and version number to the console.
#'
#' @return Version number (invisible).
#'
#' @examples
#' xegaVersion()
#' @export
xegaVersion<-function(verbose=TRUE)
{
        version<-"0.9.0.15"
        if (verbose)
	{cat('Package xega. Version', version, 'As of 2025/08/18 \n')
	cat('(c) Andreas Geyer-Schulz\n')}
        invisible(version)

}

