
#' The main program of the e(x)tended (e)volutionary and  (g)enetic (a)lgorithm
#' (xega) package.
#' 
#' @section Layers (in top-down direction):
#'
#' \enumerate{ 
#'   \item \strong{Top-level main programs}
#'         (Package \code{xega} <https://CRAN.R-project.org/package=xega>): 
#'         \code{xegaRun()}, \code{xegaReRun()}
#'   \item \strong{Population-level operations - independent of representation}
#'         (Package \code{xegaPopulation}
#'          <https://CRAN.R-project.org/package=xegaPopulation>):
#'         The population layer consists of functions for initializing,
#'         logging, observing, evaluating a population of genes,
#'         as well as computing the next population.
#'   \item \strong{Gene-level operations - representation-dependent}.
#'         \enumerate{
#'         \item 
#'         \strong{Binary representation} 
#'         (Package \code{xegaGaGene} 
#'          <https://CRAN.R-project.org/package=xegaGaGene>):
#'         Initialization of random binary genes, 
#'         several gene maps for binary genes, 
#'         several mutation operators, 
#'         several crossover operators with 1 and 2 kids, 
#'         replication pipelines for 1 and 2 kids, 
#'         and, last but not least, function factories for configuration. 
#'         \item \strong{Real-coded genes} 
#'               (Package \code{xegaDfGene}
#'                <https://CRAN.R-project.org/package=xegaDfGene>).
#'         \item \strong{Permutation genes} (Package \code{xegaPermGene}
#'                <https://CRAN.R-project.org/package=xegaPermGene>).
#'         \item \strong{Derivation-tree genes} (Package \code{xegaGpGene}
#'                <https://CRAN.project.org/package=xegaGpGene>).
#'         \item \strong{Binary genes with a grammar-driven decoder}
#'         (Package \code{xegaGeGene} 
#'                <https://CRAN.project.org/package=xegaGeGene>).
#'         }
#'   \item \strong{Gene-level operations - independent of representation}
#'         (Package \code{xegaSelectGene}
#'                <https://CRAN.project.org/package=xegaSelectGene>).
#'         Functions for static and adaptive fitness scaling,  
#'         gene selection, gene evaluation,
#'         as well as measuring performance and configuration.
#'         }
#'
#' @section Early Termination:
#'
#' A problem environment may implement a function 
#' \code{terminate(solution)} which returns TRUE 
#' if the \code{solution} meets a condition for early 
#' termination.
#'
#' @section Genetic Operator Pipelines:
#'
#' In \code{xega} the extended gene life-cycle is:
#'
#' \itemize{
#' \item (sequential) 
#' ... -> gene (data structure) -> select -> replicate(crossover -> mutate -> accept) 
#' \item (parallelizable)
#'     -> decode -> evaluate -> gene (data structure) -> ...
#' }
#'
#' Only the decode -> evaluate phase can be parallelized. 
#' The select -> crossover -> mutate -> accept phases are executed sequentially. 
#' For algorithms which require e.g. evaluation during the accept phase 
#' (simulated annealing variants), this organization implies 
#' that the improvement by parallel or distributed execution models
#' is negligible.
#'
#' A genetic operator pipeline embeds 
#' the crossover -> mutate -> accept -> decode -> evaluate phases 
#' into a function closure
#' per gene. The evaluation of a population of genes can be evaluated in parallel 
#' with potentially high reductions in total run-time.  
#'
#' The gene life-cycle with genetic operator pipelines in \code{xega} is:
#'
#' \itemize{
#' \item (sequential) 
#' ... -> gene (data structure) -> select -> replicate pipeline -> gene (function closure) 
#' \item (parallelizable)
#'     -> evaluate -> gene (data structure) ...
#' }
#'
#' This shifts the execution of the  
#' crossover -> mutate -> accept -> decode -> evaluate phases to the parallizable
#' evaluation of a function closure.
#'
#' @section Parallel and Distributed Execution:
#'
#' Several implementations of a parallel \code{lapply()} function 
#' are provided. They support
#' the parallel and distributed execution of fitness functions
#' on several combinations of hard- and software architectures.
#' A parallel \code{lapply()}-function 
#' must have the following abstract interface:
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
#' \item the function \code{parallel::mclapply()} for multi-core 
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
#' of the \code{Rmpi} package are provided for an HPC environment with Slurm
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
#' The user interface layer (package \code{xega}
#' <https://CRAN.R-project.org/package=xega> 
#' ) provides a function call interface and configuration support
#' for several algorithms: genetic algorithms (sga), 
#' permutation-based genetic algorithms (sgPerm), 
#' derivation-free algorithms as e.g. differential evolution (sgde), 
#' grammar-based genetic programming (sgp), and grammatical evolution
#' (sge). 
#'
#' \item
#' The population layer (package \code{xegaPopulation}
#' <https://CRAN.R-project.org/package=xegaPopulation> 
#' ) contains
#' population-related functionality as well as support for 
#' population statistics dependent adaptive mechanisms and 
#' for parallelization.
#'
#' \item 
#' The gene layer is split into a representation-independent and 
#' a representation-dependent part:
#' \enumerate{
#' \item 
#'  The representation-independent part 
#'  (package \code{xegaSelectGene}
#' <https://CRAN.R-project.org/package=xegaSelectGene> 
#'  )
#'  is responsible for variants of selection operators, evaluation 
#'  strategies for genes, as well as profiling and timing capabilities.        
#' \item 
#'  The representation-dependent part consists of the following packages: 
#' \itemize{
#' \item \code{xegaGaGene} 
#' <https://CRAN.R-project.org/package=xegaGaGene> 
#' for binary-coded genetic algorithms.
#' \item \code{xegaPermGene} 
#' <https://CRAN.R-project.org/package=xegaPermGene> 
#' for permutation-based genetic algorithms.
#' \item \code{xegaDfGene} 
#' <https://CRAN.R-project.org/package=xegaDfGene> 
#' for derivation-free algorithms e.g. 
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
"_PACKAGE"
