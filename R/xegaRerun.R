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
#'              a simple genetic algorithm. The problem environment and the 
#'              the grammar are extracted from the solution object.
#'
#'              If \code{script==TRUE}, the \code{solution} object and a R script 
#'              to rerun xega are written to the current directory.
#'
#' @details \code{xegaReRun()} does not capture the configuration for 
#'          parallel/distributed processing for the execution model
#'          "FutureApply", because the user defines the configuration
#'          before calling \code{xegaRun()}. 
#'
#'          If \code{executionModel} matches neither \code{"Sequential"} nor \code{"MultiCore"}
#'          or \code{!is.null(uParApply)==TRUE},   
#'          a warning is printed, and the previous solution is returned.
#'
#' @param  solution  The solution of a 
#'                   previous run of \code{xegaRun()}.
#' @param  script    Boolean. Default: FALSE. 
#'                   If TRUE, write an R script to repeat the xega run
#'                   with the command in the solution object.
#' @param  fn        Filename of R script. Default: xegaRunScript.R
#'
#' @return A list of 
#'         \enumerate{
#'         \item
#'         \code{$popStat}: A matrix with 
#'                         mean, min, Q1, median, Q3, max, var, mad
#'                          of population fitness as columns:
#'                          i-th row for i-th each generation.
#'         \item 
#'         \code{$fit}: Fitness vector if \code{generations<=1} else: NULL.
#'         \item
#'         \code{$solution}: With fields 
#'         \code{$solution$name},
#'         \code{$solution$fitness}, 
#'         \code{$solution$value},  
#'         \code{$numberOfSolutions},
#'         \code{$solution$genotype}, 
#'         \code{$solution$phenotype}, 
#'         \code{$solution$phenotypeValue}, 
#'         \item 
#'         \code{$evalFail}: Number of failures of fitness evaluations.
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
#' seqApply<-function(pop, EvalGene, lF) {lapply(pop, EvalGene, lF)}
#' c<-xegaRun(Parabola2D, max=FALSE, algorithm="sga", uParApply=seqApply)
#' b<-xegaReRun(c)
#'
#' @export
xegaReRun<-function(solution, script=FALSE, fn="xegaRunScript")
{ 
z<-solution$GAconfig[[1]]
i1<-gregexpr("penv=", z)
e1<-i1[[1]]+attr(i1[[1]], "match.length")-1
nz<-substring(z, 1, e1)
nz<-paste0(nz,"solution$GAenv$penv,grammar=solution$GAenv$grammar")
i1<-gregexpr(",max=", z)
s1<-i1[[1]]
rest<-substring(z, s1, nchar(z))
nz<-paste0(nz, rest)

if (script)
{
sfn<-paste0(fn, "solution.rds")
rfn<-paste0(fn,".R")
saveRDS(solution, file=sfn)
nz<-paste0("r<-",nz)
nz<-paste0("\n library(xega) \n solution<-readRDS(file=\"", 
           sfn, "\" ) \n\n", nz)
nz<-gsub(",", ",\n    ", nz)
writeLines(nz, con=rfn)

return(solution)
}

if (!is.null(solution$GAenv$uParApply)) 
{warning("Error: Re-run of configuration with a user supplied parallel apply not supported."); return(solution)}
if (!(solution$GAenv$executionModel %in% c("Sequential", "MultiCore", "MultiCoreHet"))) 
{warning("Error: Re-run of parallel or distributed configurations not possible."); return(solution)}

eval(parse(text=nz)) 
}


