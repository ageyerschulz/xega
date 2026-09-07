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
#'          a warning is printed, and \code{xegaReRun()} attempts the execution, but may fail, 
#'          because the set up of the parallel environment has not been performed before \code{xegaReRun}.
#'
#' @param  solution  The solution of a 
#'                   previous run of \code{xegaRun()}.
#'                   Or a configuration or a list of configurations.
#' @param  script    Boolean. Default: FALSE. 
#'                   If TRUE, write an R script to repeat the xega run
#'                   with the command in the solution object.
#' @param  fn        Filename of R script. Default: xegaRunScript.R
#'
#' @param  ...       List of named arguments of xega, you want to override.        
#'                   If solution is a configuration list, the process id must be 
#'                   be specified. It is used as an index to select the configuration. 
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
#'               tUsed, tInit, tNext, tEval, tObserve, tMigrate, and tSummary.
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
xegaReRun<-function(solution, script=FALSE, fn="xegaRunScript", ...)
{ 
iSolution<-solution

#### solution is a configuration list.
if (is.null(names(solution))) 
{
newArgList<-eval(substitute(alist(...)))
#### we pick the solution for the pid.
iSolution<-solution[[1+newArgList$pid]]
}

z<-iSolution$GAconfig[[1]]
#i1<-gregexpr("penv=", z)
#e1<-i1[[1]]+attr(i1[[1]], "match.length")-1
#nz<-substring(z, 1, e1)
nz<-paste0("xegaRun(iSolution$GAenv$penv,grammar=iSolution$GAenv$grammar")
i1<-gregexpr(",max=", z)
s1<-i1[[1]]
rest<-substring(z, s1, nchar(z))
nz<-paste0(nz, rest)

if (script)
{
sfn<-paste0(fn, "solution.rds")
rfn<-paste0(fn,".R")
saveRDS(iSolution, file=sfn)
nz<-paste0("r<-",nz)
nz<-paste0("\n library(xega) \n solution<-readRDS(file=\"", 
           sfn, "\" ) \n\n", nz)
nz<-gsub(",", ",\n    ", nz)
writeLines(nz, con=rfn)

return(iSolution)
}

if (!is.null(iSolution$GAenv$uParApply)) 
{warning("Warning: Re-run of configuration with a user supplied parallel apply may not work.") 
i1<-gregexpr(",uParApply=", nz)
i2<-gregexpr(",Cluster=", nz)
p1<-substring(nz, 1, i1)
p2<-"uParApply=iSolution$GAenv$uParApply"
p3<-substring(nz, i2, nchar(nz))
nz<-paste0(p1, p2, p3)
}

if (!identical(iSolution$GAenv$RmpiFNS, "Unused")) 
{warning("Warning: Re-run of configuration in a parallel setting may not work.") 
i1<-gregexpr(",RmpiFNS=", nz)
i2<-gregexpr(",Send=", nz)
p1<-substring(nz, 1, i1)
p2<-"RmpiFNS=iSolution$GAenv$RmpiFNS"
p3<-substring(nz, i2, nchar(nz))
nz<-paste0(p1, p2, p3)
}

if (!(iSolution$GAenv$executionModel %in% c("Sequential", "MultiCore", "MultiCoreHet"))) 
{warning("Warning: Re-run of parallel or distributed configurations may not work.")}

# replace xegaArgs in ...

nargs<-...length()

if (!nargs==0) {
# cat("Working args!\n")
xegaArgs<-names(iSolution$GAenv)
newArgList<-eval(substitute(alist(...)))
newArgs<-names(newArgList)
# cat(rep("=", 19), "\n")
# print(newArgs)
# cat(rep("=", 19), "\n")
# print(newArgList)
# cat(rep("=", 19), "\n")
for (i in (1:nargs)) 
{ ind<-(1:length(xegaArgs))[xegaArgs %in% newArgs[i]]
#   cat(i, "xega arg:", xegaArgs[ind], "\n")
  if (!0==length(ind))
  {
    pat1<-paste0(",",newArgs[i],"=")
    pat2<-paste0(",",xegaArgs[ind+1],"=")
    
    i1<-gregexpr(pat1, nz)
    i2<-gregexpr(pat2, nz)
    p1<-substring(nz, 1, i1)
 #    cat("p1", "\n")
  #   cat(p1, "\n")
    val<-unlist(newArgList[i])
    if (is.character(val)) {val<-paste0("\"", val, "\"")}
    p2<-paste0(newArgs[i], "=", val)
#     cat("p2", "\n")
#     cat(p2, "\n")
    p3<-substring(nz, i2, nchar(nz))
#     cat("p3", "\n")
#     cat(p3, "\n")
    nz<-paste0(p1, p2, p3)
  }
}
# cat(rep("=", 19), "\n")
# cat("nz\n", nz, "\n")
#return(nz)
}

eval(parse(text=nz)) 
}

