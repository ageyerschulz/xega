#
# (c) 2021 Andreas Geyer-Schulz
#          Simple Genetic Algorithm in R. V 0.1
#          Layer: Top-level main programs.     
#          Package: xega
#

#' Run an evolutionary or genetic algorithm 
#' with the same configuration as in the previous run.
#'
#' @description \code{xegaReRun()} runs a simple genetic algorithm with 
#'              the same configuration as in the run specified by the 
#'              list element \code{$GAconfig} of the solution of 
#'              a simple genetic algorithm.
#'
#' @details \code{xegaReRun()} does not capture the configuration for 
#'          parallel/distributed processing for the execution model
#'          "FutureApply", because the user defines the configuration
#'          before calling \code{xegaRun()}. 
#'
#' @param  solution  The solution of a 
#'                   previous run of \code{xegaRun()}.
#'
#' @return A list of 
#'         \enumerate{
#'         \item
#'         \code{$popStat}: A matrix with 
#'                         mean, min, Q1, median, Q3, max, var, mad
#'                          of population fitness as columns:
#'                          i-th row for i-th each generation.
#'         \item
#'         \code{$solution}: With fields 
#'         \code{$solution$fitness}, 
#'         \code{$solution$value},  
#'         \code{$solution$genotype}, and  
#'         \item
#'         \code{$GAconfig}: The configuration of the GA used by \code{xegaReRun()}.
#'         \item
#'         \code{$GAenv}:  Attribute value list of GAconfig.
#'         \item \code{$timer}: An attribute value list with 
#'               the time used (in seconds) in the main blocks of the GA:
#'               tUsed, tInit, tNext, tEval, tObserve, and tSummary.
#'         }
#'         
#' @family Main Program
#'         
#' @examples
#' a<-xegaRun(Parabola2D, max=FALSE, algorithm="sga", generations=10, popsize=20, verbose=1)
#' b<-xegaReRun(a)
#'
#' @export
xegaReRun<-function(solution)
{ eval(parse(text=solution$GAconfig)) }

