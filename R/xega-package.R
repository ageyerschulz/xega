
#' The main program of the e(x)tended (e)volutionary and  (g)enetic (a)lgorithm
#' (xega) package.
#' 
#' @section Layers (in top-down direction):
#'
#' \enumerate{ 
#'   \item \strong{Top-level main programs}
#'         (Package \code{xega}): 
#'         \code{RunGA()}, \code{ReRun()}
#'   \item \strong{Population level operations - independent of representation}
#'         (Package \code{xegaPopulation}):
#'         The population layer consists of functions for initializing,
#'         logging, observing, evaluating a population of genes,
#'         as well as of computing the next population.
#'   \item \strong{Gene level operations - representation-dependent}.
#'         \enumerate{
#'         \item 
#'         \strong{Binary representation} (Package \code{xegaGaGene}):
#'         Initialization of random binary genes, 
#'         several gene maps for binary genes, 
#'         several mutation operators, 
#'         several crossover operators with 1 and 2 kids, 
#'         replication pipelines for 1 and 2 kids, 
#'         and, last but not least, function factories for configuration. 
#'         \item \strong{Real-coded genes} (Package \code{xegaDfGene}).
#'         \item \strong{Permutation genes} (Package \code{xegaPermGene}).
#'         \item \strong{Derivation-tree genes} (Package \code{xegaGpGene}).
#'         \item \strong{Binary genes with a grammar-driven decoder}
#'         (Package \code{xegaGeGene}). 
#'         }
#'   \item \strong{Gene level operations - independent of representation}
#'         (Package \code{selectGene}):
#'         Functions for static and adaptive fitness scaling,  
#'         gene selection, and gene evaluation
#'         as well as for the measurement of performance and for configuration.
#'         }
#'
#' @section Early Termination:
#'
#' A problem environment may implement a function 
#' \code{terminate(solution)} which returns TRUE 
#' if the \code{solution} meets a condition for early 
#' termination.
#'
#' @section Parallel and Distributed Execution:
#'
#' Parallel and distributed execution is supported for
#' several combinations of hard- and software architectures
#' by overloading the \code{lapply()}-function used in the 
#' evaluation of a fitness function for a population of genes
#' with a parallel version with the abstract interface:
#'  
#' \code{parallelApply(pop, EvalGene, lF)}
#'
#' where \code{pop} is a list of genes, \code{EvalGene} the evaluation 
#' function for the fitness of a gene, and \code{lF} the local function
#' configuration of the algorithm.
#'
#' The several implementations of a \code{parallelApply()} function 
#' are provided. The implementations use
#'
#' \itemize{
#' \item the function \code{parallel::mclapply()} for multicore 
#'       parallelization by the fork mechanism of Unix-based operating systems 
#'       on a single machine.
#' \item the function \code{parallel::parLapply()} for socket connections
#'       on a single or multiple machines on the Internet.
#' \item the function \code{future.apply::future_lapply()} for 
#'       asynchronous parallelization based on future packages.
#' }
#'
#' In addition, user-defined parallel apply functions can be provided.
#' Example scripts for using the \code{Rmpi::mpi.parLapply()} function
#' of the \code{Rmpi} package are provided for a HPC environment with Slurm
#' as well as on a notebook. 
#'
#' @section The Architecture of the xegaX-Packages:
#' 
#' The xegaX-packages are a family of R-packages which implement 
#' e(x)tended (e)volutionary and (g)enetic (a)lgorithms (xega).  
#' The architecture has 3 layers, 
#' namely the user interface layer,
#' the population layer, and the gene layer: 
#' 
#' \itemize{
#' \item
#' The user interface layer (package \code{xega}) 
#' provides a function call interface and configuration support
#' for several algorithms: genetic algorithms (sga), 
#' permutation-based genetic algorithms (sgPerm), 
#' derivation free algorithms as e.g. differential evolution (sgde), 
#' grammar-based genetic programming (sgp) and grammatical evolution
#' (sge). 
#'
#' \item
#' The population layer (package \code{xegaPopulation}
#' <https://CRAN.R-project.org/package=xegaPopulation> 
#' ) contains
#' population related functionality as well as support for 
#' population statistics dependent adaptive mechanisms and 
#' for parallelization.
#'
#' \item 
#' The gene layer is split in a representation independent and 
#' a representation dependent part:
#' \enumerate{
#' \item 
#'  The representation indendent part 
#'  (package \code{xegaSelectGene}
#' <https://CRAN.R-project.org/package=xegaSelectGene> 
#'  )
#'  is responsible for variants of selection operators, evaluation 
#'  strategies for genes, as well as profiling and timing capabilities.        
#' \item 
#'  The representation dependent part consists of the following packages: 
#' \itemize{
#' \item \code{xegaGaGene} 
#' <https://CRAN.R-project.org/package=xegaGaGene> 
#' for binary coded genetic algorithms.
#' \item \code{xegaPermGene} 
#' <https://CRAN.R-project.org/package=xegaPermGene> 
#' for permutation-based genetic algorithms.
#' \item \code{xegaDfGene} 
#' <https://CRAN.R-project.org/package=xegaDfGene> 
#' for derivation free algorithms as e.g. 
#'                         differential evolution.
#' \item \code{xegaGpGene} 
#' <https://CRAN.R-project.org/package=xegaGpGene> 
#' for grammar-based genetic algorithms.
#' \item \code{xegaGeGene} 
#' <https://CRAN.R-project.org/package=xegaGaGene> 
#' for grammatical evolution algorithms.
#' }
#' The packages \code{xegaDerivationTrees} and \code{xegaBNF} support
#' the packages \code{xegaGpGene} and \code{xegaGeGene}:
#' \itemize{
#' \item \code{xegaBNF} 
#' <https://CRAN.R-project.org/package=xegaBNF> 
#' essentially provides a grammar compiler and
#' \item 
#' \code{xegaDerivationTrees} 
#' <https://CRAN.R-project.org/package=xegaDerivationTrees> 
#' an abstract data type for derivation trees.
#' }
#' }} 
#' 
#' @family Package Description
#'
#' @name xega
#' @aliases xega
#' @docType package
#' @title Package xega
#' @author Andreas Geyer-Schulz
#' @section Copyright: (c) 2023 Andreas Geyer-Schulz
#' @section License: MIT
#' @section URL: https://github.com/ageyerschulz/xega 
#' @section Installation: From CRAN by \code{install.packages('xega')} 
NULL

