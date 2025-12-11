#
# (c) 2021 Andreas Geyer-Schulz
#          Simple Genetic Algorithm in R. V 0.1
#          Layer: Top-level main programs.     
#          Package: xega
#

#' Run an evolutionary or genetic algorithm for a problem environment 
#' which contains a function to optimize. 
#'
#' @description \code{xegaRun()} runs an evolutionary or genetic algorithm 
#'        whose type is selected by \code{algorithm}. Available
#'        algorithms are:
#'        \enumerate{
#'        \item \code{"sga"}:    Genetic algorithm with binary genes.
#'        \item \code{"sgde"}:   Differential evolution with real genes.
#'        \item \code{"sgperm"}: Genetic algorithm with permutation genes. 
#'        \item \code{"sgp"}:    Grammar-based genetic programming with 
#'                               derivation-tree genes.
#'        \item \code{"sge"}:    Grammatical evolution (genetic algorithm 
#'                               with binary genes and a grammar-driven
#'                               decoder).
#'        \item \code{"sgede"}:  Grammatical evolution (genetic algorithm 
#'                               with real genes, genetic operators from 
#'                               from differential evolution 
#'                               and a grammar-driven
#'                               decoder). 
#'        }
#'   
#'       The choice of the algorithm determines the gene-dependent 
#'       configuration options.
#'
#' @details  The algorithm expects a problem environment \code{penv} which is a 
#'           named list with at least the following functions:
#'         \itemize{
#'      \item \code{$name()}:      The name of the problem environment.
#'      \item \code{$f(parm, gene=0, lF=0)}:   The function to optimize.
#'                                  The parameters gene and lF are provided
#'                                  for future extensions.
#'         }
#'   
#'      Additional parameters needed depend on the algorithm 
#'      and the problem environment. 
#'      For example, for binary genes for function optimization,    
#'      additional elements must be provided:
#'
#'         \itemize{
#'      \item \code{$bitlength()}: The vector of the 
#'                                 bitlengths of the parameters.  
#'      \item \code{$genelength()}: The number of bits of a gene.  
#'      \item \code{$lb()}:         The vector of lower bounds 
#'                                 of the parameters. 
#'      \item \code{$ub()}:  The vector of upper bounds of the parameters. 
#'          }
#'
#' @section Problem Specification:
#'
#' The problem specification consists of 
#' \itemize{ 
#' \item \code{penv}: The problem environment. 
#' \item \code{max}:  Maximize? Boolean. Default: \code{TRUE}.
#' \item \code{grammar}: A grammar object. For the algorithms \code{"sgp"} and \code{"sge"}.   
#' }
#'
#' @section Basic Parameters:
#'
#' The main parameters of a ``standard'' genetic algorithm are:
#'    \itemize{
#'      \item \code{popsize}:     Population size.
#'      \item \code{generations}: Number of generations.
#'      \item \code{crossrate}:   Constant probability of one-point crossover.
#'      \item \code{mutrate}:     Constant probability of mutation.
#'          }
#'
#'  \code{crossrate} and \code{mutrate} specify the probability of 
#'  applying the genetic operators crossover and mutation to a gene.
#'
#' Two more parameters are important:
#'
#' \itemize{
#' \item \code{elitist}: Boolean. If \code{TRUE} (default), the fittest gene always survives.
#' \item \code{replay}:  Integer. If \code{0} (default), a random seed of the random number generator is chosen.
#'                       For exact replications of a run of a genetic algorithm, set replay to a positive integer.
#' }
#'
#' @section Global and Local Parameters:
#'
#' However, when using uniform crossover instead of one-point crossover, 
#' an additional parameter which specifies the probability of taking a bit 
#' from the first parent becomes necessary. 
#' Therefore, we distinguish between global and local operator parameters:
#' \enumerate{
#' \item Global operator parameters: 
#'            The probabilities of applying a crossover (\code{crossrate}) or 
#'            a mutation operator (\code{mutrate}) to a gene.
#' \item Local operator parameters: 
#'       E.g. the per-bit probability of mutation or the probability
#'       of taking a bit from parent 1 for the uniform crossover operator.
#'       Local operator parameters affect only 
#'       the genetic operator which needs them.
#' }
#'
#' There exist several advantages of this classification of parameters:
#' \itemize{
#' \item For the formal analysis of the behavior of the algorithms, 
#'       we achieve a division in two parts: The equations of the 
#'       global parameters with operator-specific expressions as plug-ins. 
#' \item For empirically finding parameterizations for problem classes, 
#'       we propose to fix local parameters at reasonable values
#'       (e.g. based on biological evidence)
#'       and conditional on this optimize the (few) remaining global 
#'       parameters.
#' \item For parallelization, specialized 
#'       gene processing pipelines can be built and more efficiently executed, 
#'       because the global parameters \code{crossrate} and \code{mutrate} decide 
#'       which genes survive 
#' \enumerate{
#'    \item unchanged, 
#'    \item mutated, 
#'    \item crossed, and 
#'    \item crossed as well as mutated. 
#' }}
#' 
#' To mimic a classic genetic algorithm with crossover and bit mutation rate, 
#' the probability of applying the mutation operator to a gene 
#' should be set to \code{mutrate=1}.
#'
#' @section Global Adaptive Mechanisms:
#'
#' The adaptive mechanisms described in the following are based on threshold
#' rules which determine how a parameter of the genetic operator is adapted.
#' The threshold conditions are based on population statistics: 
#'
#' \strong{Adaptive Scaling.} For adaptive scaling, select a dynamic scaling method,
#'                   e.g. \code{scaling="ThresholdScaling"}.
#' A high selection pressure decreases the dispersion in the population.
#' The parameter \code{scalingThreshold} is a numerical parameter which defines    
#' an interval from \code{1-scalingThreshold} to \code{1+scalingThreshold}:
#' \enumerate{
#' \item If the RDM is in this interval, the fitness function is not scaled. 
#' \item If the RDM is larger than the upper bound of the interval, 
#'       the constant \code{scalingExp} which is higher than \code{1} is chosen for the scaling function.
#'       This implements the rule: If the dispersion has increased, increase the selection pressure.  
#' \item If the RDM is smaller than the lower bound of the interval, 
#'       the constant \code{scalingExp2} which is smaller than \code{1} is chosen for the scaling function.
#'       This implements the rule: If the dispersion has decreased, increase the selection pressure.  
#'       }
#'
#'  The dispersion measure is computed as the ratio of the dispersion measure at \code{t} relative to the 
#'  dispersion measure at \code{t-scalingDelay}. 
#'  The default dispersion measure is the variance of the population fitness (\code{dispersionMeasure="var"}). 
#'  However, other dispersion measures ("std", "mad", "cv", "range", "iqr") can be configured.  
#'
#'  Another adaptive scaling method is continuous scaling (\code{scaling="ContinuousScaling"}).
#'  The scaling exponent is adapted by a weighted ratio of dispersion measures. The weight 
#'  of the exponent is set by \code{rdmWeight=1.1}, its default is \code{1.0}. Since the ratio 
#'  of dispersion measures may be quite unstable, the default limits for the ratio are \code{drMin=0.5} 
#'  and \code{drMax=2.0}. 
#'
#' \strong{Individually Variable Mutation and Crossover Probabilities}
#'
#' The rationale of individually variable mutation and crossover rates is that selected genes 
#' with a low fitness should be changed by a genetic operator with a higher probability. 
#' This increases the chance of survival of the gene because of the chance of a fitness increase through  
#' crossover or mutation.
#'
#' Select an adaptive genetic operator rate:
#' For the crossover rate, \code{ivcrossrate="IV"}. For the mutation rate, \code{ivmutrate="IV"}.
#'
#' If the fitness of a gene is higher than \code{cutoffFit} times the current best fitness, 
#' the crossover rate is \code{crossrate} else the crossover rate is \code{crossrate2}.
#'
#' If the fitness of a gene is higher than \code{cutoffFit} times the current best fitness, 
#' the mutation rate is \code{mutrate} else the mutation rate is \code{mutrate2}.
#'
#' @section The Initialization of a Population:
#'
#' For the algorithms "sga", "sgde", and "sgperm" the information needed for
#' initialization is the length of the gene in bits, in parameters, and in
#' the number of symbols of a permutation. 
#' For "sgp", the depth bound gives an upper limit for the 
#' program which can be represented by a derivation tree.
#' For "sge", a codon is an integer for selecting a production rule.
#' The number of bits of a gene is \code{codons*codonBits}. 
#'
#' \tabular{lll}{
#' \strong{Algorithm}\tab \tab \strong{Parameters} \cr
#' \strong{"sga"}\tab Number of bits.  \tab \code{penv$genelength()} \cr
#' \strong{"sgde"}\tab Number of parameters. \tab 
#'                     \code{length(penv$bitlength()}, 
#'                     \code{penv$lb()}, \code{penv$ub()}\cr
#' \strong{"sgede"}\tab Number of Codons. \tab 
#'                     \code{codons}, \code{codonPrecision}\cr
#' \strong{"sgperm"}\tab Number of symbols. \tab \code{penv$genelength()} \cr
#' \strong{"sgp"}\tab Depth bound of derivation tree. \tab \code{maxdepth} \cr
#' \strong{"sge"}\tab Number of codons and 
#'    \tab\code{codons}, \code{codonBits},
#'                   \code{codonPrecision}, \code{maxPBias} \cr
#' \tab number of bits of a codon. \tab
#' }
#'
#' @section The Pipeline of Genetic Operators:
#'
#' The pipeline of genetic operators merges the pipeline of a genetic algorithm with the pipeline of 
#' evolutionary algorithms and simulated annealing by adding an acceptance step: 
#'
#' \itemize{
#' \item For evolutionary algorithms,
#' the acceptance rule \code{accept="Best"} means that the fitter gene out of a parent and its kid survives
#' (is copied into the next generation).
#' \item For genetic algorithms the acceptance rule \code{accept="All"} means that always the kid survives.
#' \item For simulated annealing the acceptance rule \code{accept="Metropolis"} 
#' means that the survival probability of a kid with a fitness
#' worse than its parent decreases as the number of generations executed increases. 
#' \item The evaluation of the operator pipeline can be shifted to the 
#'       evaluation phase by pipeline compilation: 
#'       \itemize{
#'       \item \code{pipeline="PipeC"}: 
#'       (population of function closures of genetic operator pipelines)
#'       or 
#'       \item \code{pipeline="PipeG"}:
#'       (population of genes with embedded genetic operator pipelines).
#'       }
#' }
#'
#' Proper configuration of the pipeline allows the configuration of new algorithm variants which mix elements
#' of genetic, evolutionary, and simulated annealing algorithms.
#'
#' The following table gives a working standard configuration of the pipeline of the genetic operators for each 
#' of the five algorithms:
#'
#' \tabular{lccc}{
#' \strong{Step/Algorithm}\tab\strong{"sga"}\tab\strong{"sgde"}\tab\strong{"sgperm"}\cr 
#' (next) Scaling         \tab NoScaling    \tab NoScaling     \tab NoScaling       \cr
#' (next) Selection       \tab  SUS         \tab  UniformP     \tab SUS             \cr
#' (next) Replication     \tab  Kid2        \tab    DE         \tab   Kid2          \cr
#' (next) Crossover       \tab  Cross2Gene  \tab  UCrossGene   \tab  Cross2Gene     \cr
#' (next) Mutation        \tab  MutateGene  \tab  MutateGeneDE \tab  MutateGene     \cr
#' (next) Acceptance      \tab    All       \tab   Best        \tab   All           \cr 
#' (eval) Decoder         \tab   Bin2Dec    \tab Identity      \tab Identity        \cr
#' (eval) Evaluation      \tab  EvalGeneU   \tab EvalGeneU     \tab EvalGeneU      
#' }
#'
#' \tabular{lccc}{
#' \strong{Step/Algorithm}\tab\strong{"sgp"}\tab\strong{"sge"} \tab\strong{"sgede"} \cr 
#' (next) Scaling         \tab NoScaling    \tab NoScaling     \tab NoScaling       \cr
#' (next) Selection       \tab   SUS        \tab  SUS          \tab UniformP        \cr
#' (next) Replication     \tab   Kid2       \tab  Kid2         \tab    DE           \cr
#' (next) Crossover       \tab Cross2Gene   \tab Cross2Gene    \tab UCrossGene      \cr
#' (next) Mutation        \tab MutateGene   \tab MutateGene    \tab MutateGeneDE    \cr
#' (next) Acceptance      \tab    All       \tab   All         \tab   Best          \cr 
#' (eval) Decoder         \tab     -        \tab   Mod         \tab Identity        \cr
#' (eval) Evaluation      \tab  EvalGeneU   \tab EvalGeneU     \tab EvalGeneU
#' }
#' 
#' @section Scaling:
#' 
#' In genetic algorithms, scaling of the fitness functions has the purpose of increasing or decreasing 
#' the selection pressure. Two classes of scaling methods are available:
#'
#' \itemize{
#' \item Constant scaling methods.
#' \itemize{
#' \item No scaling (configured by \code{scaling="NoScaling"}).
#' \item Constant scaling (configured by \code{scaling="ConstantScaling"}).
#'       Depends on the scaling exponent \code{scalingExp}. 
#' }
#' \item Adaptive scaling methods.
#' \itemize{      
#' \item Threshold scaling (configured by \code{scaling="ThresholdScaling"}).
#'       It is configured with the scaling exponents \code{scalingExp} and \code{scalingExp2}, 
#'       and the scaling threshold \code{scalingThreshold}.
#'       It uses a threshold rule about the change of a dispersion measure 
#'       of the population fitness \code{lF$RDM()} 
#'       to choose the scaling exponent:
#'       \itemize{
#'       \item \code{lF$RDM()>1+scalingThreshold}: The scaling exponent is \code{scalingExp} 
#'              which should be greater than \code{1}. 
#'             Rationale: Increase selection pressure to reduce the dispersion of fitness.
#'       \item \code{lF$RDM()<1-scalingThreshold}: The scaling exponent is \code{scalingExp2} 
#'             which should be lower than \code{1}.
#'             Rationale: Decrease selection pressure to increase the dispersion of fitness.
#'       \item Else: Scaling exponent is \code{1}. Fitness is not scaled.  
#'       }
#' \item Continuous scaling (configured by \code{scaling="ContinuousScaling"}).
#'         The ratio of the dispersion measures \code{lF$RDM()} is 
#'         greater than 1 if the dispersion increased in the last generation and 
#'         less than 1 if the dispersion decreased in the last generation. 
#'         The scaling exponent is the product of the ratio of the 
#'         dispersion measures \code{lF$RDM()} with the 
#'         weight \code{rdmWeight}. 
#' }
#' }
#'
#' The change of the dispersion measure of the population fitness is measured by the function \code{lF$RDM()}
#' (RDM means (R)atio of (D)ispersion (M)easure). This function depends on
#' \itemize{
#' \item the choice of a dispersion measure of the population fitness \code{dispersionMeasure}. 
#'       The variance is the default (\code{dispersionMeasure="var"}).
#'       The following dispersion measures of the population fitness are avalaible:
#'       Variance (\code{"var"}), 
#'       standard deviation (\code{"std"}), 
#'       median absolute deviation (\code{"mad"}), 
#'       coefficient of variation (\code{"cv"}), 
#'       range (\code{"range"}), 
#'       interquartile range (\code{"iqr"}). 
#' \item the scaling delay \code{scalingDelay}. The default is \code{scalingDelay=1}. 
#'       This means the ratio of the variance of the fitness of the population at time t 
#'       and the variance of the fitness of the population at time t-1 is computed.
#' \item the upper and lower bounds of the ratio of dispersion measures. 
#' \item Dispersion ratios may have extreme fluctuations: The parameters \code{drMax} and \code{drMin}  
#'       define upper and lower bounds of the ratio of dispersion measures. 
#'       The defaults are \code{drMax=2} and \code{drMin=1}.
#' }
#' See package \code{xegaSelectGene} <https://CRAN.R-project.org/package=xegaSelectGene>
#'
#' @section Selection:
#'
#' Selection operators determine which genes are chosen for the replication process for the next generation.
#' Selection operators are configured by \code{selection} and \code{mateselection} 
#' (the 2nd parent for crossover). The default operator is stochastic universal selection 
#' for both parents (configured by \code{selection="SUS"} and \code{mateselection="SUS"}).  
#' The following operators are implemented:
#' \itemize{
#' \item Uniform random selection with replacement (configured by \code{"Uniform"}).
#'       Needed for simulating uniform random mating behavior, for computer experiments without
#'       selection pressure, and for computing random search solutions as naive benchmarks.
#' \item Uniform random selection without replacement (configured by \code{"UniformP"}).
#'       Needed for differential evolution.
#' \item Selection proportional to fitness 
#' (in \code{O(n)} by \code{"SelectPropFit"}, in \code{O(n*log(n))} by \code{"SelectPropFitOnln"}, 
#' and in \code{O(n^2)} by \code{"SelectPropFitM"}).  
#' \code{offset} configures the shift of the fitness vector if \code{min(fit)=<0}.
#' \item Selection proportional to fitness differences
#' (in \code{O(n)} by \code{"SelectPropFitDiff"}, in \code{O(n*log(n))} by \code{"SelectPropFitDiffOnln"}, 
#' and in \code{O(n^2)} by \code{"SelectPropFitDiffM"}). 
#' Even the worst gene should have a minimal chance of survival: \code{eps} is added to the 
#' fitness difference vector. This also guarantees numerical stability for populations 
#' in which all genes have the same fitness. 
#' \item Deterministic tournament selection of \code{k} genes (configured by \code{"Tournament"}).  
#'       The tournament size is configured by \code{tournamentSize}.
#'       Selection pressure increases with tournament size. 
#'       The worst \code{k-1} genes of a population never survive.
#' \item Deterministic tournament selection of \code{2} genes (configured by \code{"Duel"}).  
#' \item Stochastic tournament selection of \code{k} genes (configured by \code{"STournament"}).  
#'       The tournament size is configured by \code{tournamentSize}.
#' \item Linear rank selection with selective pressure (configured by \code{"LRSelective"}). 
#'       The selection bias which regulates the selection pressure 
#'       is configured by \code{selectionBias} 
#'       (should be between \code{1.0} (uniform selection) and \code{2.0}). 
#' \item Linear rank selection with interpolated target sampling rates (configured by \code{"LRTSR"}).
#'       The maximal target sampling rate is configured by \code{maxTSR} 
#'       (should be between \code{1} and \code{2}).
#' \item Stochastic universal sampling (configured by \code{"SUS"}).
#' }
#'
#' If \code{selectionContinuation=TRUE}, then selection functions are computed exactly once 
#' per generation. They are transformed into lookup functions which deliver the index of selected genes by 
#' indexing a vector of integers.
#'
#' See package \code{xegaSelectGene} <https://CRAN.R-project.org/package=xegaSelectGene>
#'
#' @section Replication:
#'
#' For genetic algorithms ("sga", "sgp", sgperm", and "sge") 
#' in the replication process of a gene the crossover operator may 
#' by configured to produce 
#' \enumerate{
#' \item \strong{one} new gene (\code{replication="Kid1"}, 
#' \code{replication="Kid1Pipeline"}, or \code{replication="Kid1PipelineG"})  
#' \item or \strong{two} new genes (\code{replication="Kid2"},
#' \code{replication="Kid2Pipeline"}, or \code{replication="Kid2PipelineG"}). 
#' }
#' The first version  
#' loses genetic information in the crossover operation, 
#' whereas the second version 
#' retains the genetic material in the population.
#' There is a dependency between \code{replication} and \code{crossover}:
#' \code{"Kid2"}, \code{Kid2Pipeline}, and \code{Kid2PipelineG} require a crossover operator which produces two kids.
#' The replication method is configured by the function  
#' \code{xegaGaReplicationFactory()} of package \code{xegaGaGene}.
#'
#' For differential evolution (algorithm "sgde") and grammatical evolution with 
#' differential evolution operators (algorithm "sgede"), one of
#' \itemize{
#' \item \code{replication="DE", pipeline="NoPipe"} or 
#' \item \code{replication="DEPipeline", pipeline="PipeC"} or 
#' \item \code{replication="DEPipelineG", pipeline="PipeG"} 
#' }
#' must be configured.
#'
#' The replication method for differential evolution is configured by the function  
#' \code{xegaDfReplicationFactory()} of package \code{xegaDfGene}.
#' It implements a configurable acceptance rule. For classic differential evolution, 
#' use \code{accept="Best"}. 
#'
#' The pipeline pipeline compilation (configured by \code{pipeline="PipeC"} or \code{pipeline="PipeG"})  
#' builds either a population of function closures of genetic operator pipelines
#' or a population of genes with embedded genetic operator pipelines.
#' As a consequence, the complete genetic mechanism except the selection of genes 
#' can be parallelized. 
#'
#' @section Crossover:
#'
#' The table below summarizes the crossover operators available in the current version.
#'
#' \tabular{lllll}{
#' \strong{Algorithm:} \tab \strong{"sga"} and \strong{"sge"}  \tab \strong{Package:}   \tab  \strong{xegaGaGene}  \tab\cr
#'  Kids \tab Name  \tab Function \tab crossover=  \tab Influenced by\cr
#'  (2 kids)  \tab 1-Point              \tab  xegaGaCross2Gene()            \tab "Cross2Gene"   \tab \cr
#'            \tab Uniform              \tab  xegaGaUCross2Gene()           \tab "UCross2Gene"  \tab \cr
#'            \tab Parametrized Uniform \tab xegaGaUPCross2Gene()           \tab "UPCross2Gene" \tab ucrossSwap \cr
#'  (1 kid)   \tab 1-Point              \tab xegaGaCrossGene()              \tab "CrossGene"   \tab           \cr
#'            \tab Uniform              \tab xegaGaUCrossGene()             \tab "UCrossGene"  \tab      \cr
#'            \tab Parametrized Uniform \tab  xegaGaUPCrossGene()           \tab "UPCrossGene" \tab ucrossSwap  \cr
#'  \strong{Algorithm:}  \tab \strong{"sgde"} and \strong{"sgede"} \tab \strong{Package:}    \tab \strong{xegaDfGene}  \tab \cr
#'  (1 kid)   \tab 1-Point              \tab  xegaDfCrossGene()             \tab "CrossGene"   \tab           \cr
#'            \tab Uniform              \tab  xegaDfCrossGene()             \tab "UCrossGene"  \tab      \cr
#'            \tab Parametrized Uniform \tab  xegaDfUPCrossGene()           \tab "UPCrossGene" \tab ucrossSwap  \cr
#' \strong{Algorithm:}  \tab \strong{"sgperm"} \tab \strong{Package:}    \tab \strong{xegaPermGene}   \tab \cr
#'  (2 kids)  \tab Position-Based       \tab  xegaPermCross2Gene()          \tab "Cross2Gene"   \tab \cr
#'  (1 kid)   \tab Position-Based       \tab  xegaPermCrossGene()           \tab "CrossGene"   \tab           \cr
#' \strong{Algorithm:}  \tab \strong{"sgp"} \tab \strong{Package:}    \tab \strong{xegaGpGene}    \tab \cr
#'  (2 kids)  \tab of Derivation Trees  \tab  xegaGpAllCross2Gene()       \tab "Cross2Gene" or \tab maxcrossdepth, \cr
#'            \tab                      \tab                              \tab "All2Cross2Gene" \tab maxdepth, \cr
#'            \tab                      \tab                              \tab                  \tab and maxtrials \cr
#'            \tab of Depth-Filtered    \tab  xegaGpFilterCross2Gene()    \tab "FilterCross2Gene" \tab maxcrossdepth,\cr
#'            \tab Derivation Trees     \tab                              \tab                    \tab mincrossdepth, \cr
#'            \tab                      \tab                              \tab                    \tab maxdepth, \cr
#'            \tab                      \tab                              \tab                    \tab and maxtrials \cr
#'  (1 kid)   \tab of Derivation Trees  \tab  xegaGpAllCrossGene()       \tab "AllCrossGene" \tab maxcrossdepth, \cr
#'            \tab                      \tab                              \tab            \tab maxdepth, \cr
#'            \tab                      \tab                              \tab            \tab and maxtrials \cr
#'            \tab of Depth-Filtered    \tab  xegaGpFilterCrossGene()    \tab "FilterCrossGene" \tab maxcrossdepth, \cr
#'            \tab Derivation Trees     \tab                              \tab                    \tab mincrossdepth, \cr
#'            \tab                      \tab                              \tab                    \tab maxdepth, \cr
#'            \tab                      \tab                              \tab                    \tab and maxtrials \cr
#' }
#'
#' @section Mutation:
#'
#' The table below summarizes the mutation operators in the current version.
#'
#' \tabular{llll}{
#' \strong{Algorithm:} \tab \strong{"sga"} and \strong{"sge"}  \tab \strong{Package:}   \tab  \strong{xegaGaGene} \cr
#'  Name  \tab Function \tab mutation=  \tab Influenced by\cr
#'  Bit Mutation         \tab  xegaGaMutateGene()            \tab "MutateGene"   \tab bitmutrate \cr
#'  Individually        \tab  xegaGaIVAdaptiveMutateGene()  \tab "IVM"  \tab bitmutrate,     \cr
#'  Variable Bit        \tab                                \tab        \tab bitmutrate2,     \cr
#'  Mutation            \tab                                \tab        \tab  and cutoffFit     \cr
#'  \strong{Algorithm:}  \tab \strong{"sgde"} and \strong{"sgede"} \tab \strong{Package:}    \tab \strong{xegaDfGene} \cr
#'  Differential \tab  xegaDfMutateGeneDE()             \tab "MutateGene" or   \tab lF$ScaleFactor() \cr
#'  Evolution Mutation             \tab                                     \tab "MutateGeneDe"    \tab (Configurable)   \cr
#' \strong{Algorithm:} \tab \strong{"sgperm"} \tab \strong{Package:}    \tab \strong{xegaPermGene}\cr
#'  Generalized Order  \tab  xegaPermMutateGeneOrderBased()          \tab "MutateGene"             \tab bitmutrate \cr
#'  Based Mutation     \tab                                \tab "MutateGeneOrderBased"   \tab \cr
#'  k Inversion  \tab  xegaPermMutateGenekInversion()          \tab "MutateGenekInversion" \tab lambda \cr
#'  Mutation     \tab                                \tab          \tab \cr
#'  2-Opt Mutation \tab  xegaPermMutateGene2Opt()          \tab "MutateGene2Opt" \tab max2opt \cr
#'  k-Opt LK Mutation \tab  xegaPermMutateGenekOptLK()          \tab "MutateGenekOptLK" \tab max2opt \cr
#'  (Lin-Kernighan)     \tab                                \tab          \tab \cr
#'  Greedy Path  \tab  xegaPermMutateGeneGreedy()          \tab "MutateGeneGreedy" \tab lambda \cr
#'  Mutation     \tab                                \tab          \tab \cr
#'  Best Greedy Path  \tab  xegaPermMutateGeneBestGreedy()          \tab "MutateGeneBestGreedy" \tab lambda \cr
#'  Mutation     \tab                                \tab          \tab \cr
#'  Random Mutation  \tab  xegaPermMutateMix()          \tab "MutateGeneMix" \tab  \cr
#'  Operator     \tab                                \tab          \tab \cr
#' \strong{Algorithm:}  \tab \strong{"sgp"} \tab \strong{Package:}    \tab \strong{xegaGpGene} \cr
#'  Derivation Tree  \tab  xegaGpMutateAllGene()       \tab "MutateGene" or \tab maxmutdepth \cr
#'  Mutation         \tab                              \tab "MutateAllGene" \tab  \cr
#'  Filtered Derivation   \tab  xegaGpMutateGeneFilter()       \tab "MutateFilterGene" \tab maxmutdepth, \cr
#'  Tree Mutation         \tab                              \tab                 \tab  minmutinsertiondepth, \cr
#'                       \tab                              \tab                 \tab  and maxmutinsertiondepth \cr
#' }
#'
#' @section Acceptance:
#'
#' Acceptance rules are extensions of genetic and evolutionary algorithms 
#' which - to the best of my knowledge - have their origin in simulated annealing.  
#' An acceptance rule compares the fitness value of a modified gene with the 
#' fitness value of its parent and determines which of the two genes is passed
#' into the next population.
#' 
#' An acceptance rule is only executed as part of the genetic operator pipeline, if 
#' \code{replicate="Kid1"}, 
#' \code{replicate="Kid1Pipeline"}, 
#' \code{replicate="Kid1PipelineG"},
#' \code{replicate="Kid2Pipeline"}, 
#' \code{replicate="DE"}.
#' \code{replicate="DEPipeline"},
#' or \code{replicate="DEPipelineG"}.
#' 
#' Two classes of acceptance rules are provided: 
#' \itemize{
#' \item Simple acceptance rules.   
#' \itemize{
#' \item Accept the new gene unconditionally (configured by \code{accept="All"}).
#'       The new gene is always passed to the next population. 
#'       Choose the rule for configuring a classic genetic algorithm.
#'       (The default). 
#' \item Accept only the best gene (configured by \code{accept="Best"}).
#'       This acceptance rule guarantees an increasing fitness curve over the run 
#'       of the algorithm. For example, classic differential evolution uses this acceptance rule.
#' }
#' \item Configurable acceptance rules. 
#'       The rules always accept a new gene with a fitness improvement. 
#'       They also accept a new gene with a lower fitness with a probability which depends 
#'       on the fitness difference of the old and the new gene 
#'       and a temperature parameter which is reduced over the algorithm 
#'       run by a configurable cooling schedule. 
#' \itemize{
#' \item The Metropolis acceptance rule (configured by \code{accept="Metropolis"}). 
#'       The larger the parameter \code{beta} is set, the faster the drop in acceptance probability. 
#' \item The individually adaptive Metropolis acceptance rule (configured by \code{accept="IVMetropolis"}). 
#'       The larger the parameter \code{beta} is set, the faster the drop in acceptance probability. 
#'       Individually adaptive means that the temperature is corrected. The correction (increase) of temperature 
#'       depends on the difference between the fitness of the currently known best solution
#'       and the fitness of the new gene.
#' }
#' }
#'
#' The cooling schedule updates the temperature parameter at the end of the main loop.
#' The following cooling schedules are available:
#' \itemize{ 
#' \item Exponential multiplicative cooling (configured by \code{cooling="ExponentialMultiplicative"}).
#'       Depends on the discount factor \code{alpha} 
#'       and the start temperature \code{temp0}.
#' \item Logarithmic multiplicative cooling (configured by \code{cooling="LogarithmicMultiplicative"}).
#'       Depends on the scaling factor \code{alpha} 
#'       and the start temperature \code{temp0}.
#' \item Power multiplicative cooling (configured by \code{cooling="PowerMultiplicative"}).
#'       Depends on the scaling factor \code{alpha}, 
#'       the cooling power exponent \code{coolingPower}, 
#'       and the start temperature \code{temp0}.
#' \item Power additive cooling (configured by \code{cooling="PowerAdditive"}).
#'       Depends on the number of generations \code{generations}, 
#'       the cooling power exponent \code{coolingPower}, 
#'       the start temperature \code{temp0}, and the final temperature \code{tempN}.
#' \item Exponential additive cooling (configured by \code{cooling="ExponentialAdditive"}).
#'       Depends on the number of generations \code{generations}, the 
#'       start temperature \code{temp0}, and the final temperature \code{tempN}.
#' \item Trigonometric additive cooling (configured by \code{cooling="TrigonometricAdditive"}).
#'       Depends on the number of generations \code{generations}, the 
#'       start temperature \code{temp0}, and the final temperature \code{tempN}.
#' }
#'
#' See package \code{xegaPopulation} <https://CRAN.R-project.org/package=xegaPopulation>
#'
#' @section Decoder:
#'
#' Decoders are algorithm and task-dependent. Their implementation often makes use of a gene map. 
#' The table below summarizes the available decoders 
#' and gene maps of the current version.
#'
#' \tabular{lccc}{
#' Algorithm:          \tab\strong{"sga"}          \tab\strong{"sgde"}         \tab\strong{"sgperm"}     \cr 
#' In package:         \tab xegaGaGene             \tab xegaDfGene             \tab xegaPermGene         \cr
#' Decoder:            \tab xegaGaDecodeGene()     \tab xegaDfDecodeGene()     \tab xegaPermDecodeGene() \cr
#' Gene map factories: \tab xegaGaGeneMapFactory() \tab xegaDfGeneMapFactory() \tab (Not configurable)   \cr
#' Method              \tab "Bin2Dec"               \tab "Identity"             \tab                      \cr
#' Method              \tab "Gray2Dec"              \tab                        \tab                      \cr
#' Method              \tab "Identity"              \tab                        \tab                      \cr
#' Method              \tab "Permutation"           \tab                        \tab                      \cr
#' }
#'
#' \tabular{lccc}{
#' Algorithm:          \tab \strong{"sgp"}     \tab\strong{"sge"}              \tab\strong{"sgede"}           \cr 
#' In package:         \tab xegaGpGene         \tab xegaGeGene                 \tab xegaGeGene                \cr
#' Decoder Factories   \tab (Not configurable) \tab xegaGeDecodeGeneFactory()  \tab xegaGeDecodeGeneFactory() \cr
#' Decoder:            \tab xegaGpDecodeGene() \tab                            \tab                           \cr 
#' Method:             \tab                    \tab "DecodeGene"               \tab "DecodeGene"              \cr
#' Method:             \tab                    \tab "DecodeGeneDT"             \tab "DecodeGeneDT"            \cr
#' Gene map factories: \tab (Not configurable) \tab xegaGeGeneMapFactory()     \tab xegaDfGeneMapFactory()    \cr
#' Method              \tab                    \tab "Mod"                      \tab "Identity"                \cr
#' Method              \tab                    \tab "Buck"                     \tab                           \cr
#' }
#'
#' @section Evaluation:
#'
#' The method of evaluation of a gene is configured by
#' \code{evalmethod}: \code{"EvalGeneU"} means that the function is always executed,
#  "EvalGeneR" allows repairs a gene by a decoder (e.g. in grammatical evolution), 
#' \code{"Deterministic"} evaluates a gene only once, and 
#' \code{"Stochastic"} incrementally updates the mean and 
#' variance of a stochastic function. 
#' If \code{reportEvalErrors==TRUE}, evaluation failures are reported. However, for grammatical  
#' evolution without gene repair this should be set to \code{FALSE}. 
#' See package \code{xegaSelectGene} <https://CRAN.R-project.org/package=xegaSelectGene>
#'
#' @section The Concept of Genetic Operator Pipelines:
#'
#' In the gene life cycle, a gene is 
#' \enumerate{
#'   \item  modified by the genetic machinery and then 
#'   \item  evaluated (expressed as a phenotype and 
#'          its fitness measured with regard to an environment).
#'   }   
#'
#' When using \code{xegaRun()} with default parameters,  the genetic operations 
#' are evaluated sequentially, whereas the fitness evaluation can be 
#' parallelized. The drawback of this is that for algorithms as e.g. 
#' differential evolution that in order to accept a modified gene in the 
#' population, its performance has to be better than that of its parent.
#' The comparison of the fitness of the parent gene with the fitness of its kid
#' requires an evalution of the fitness as part of the genetic operations.
#' However, this implies that for such algorithms the sequential part dominates 
#' the execution times and the benefits from parallelization remain marginal.
#'
#' Genetic operator pipelines are 
#' \enumerate{
#' \item either implemented as function closures which embed 
#' a sequence of basic genetic operations \code{pipeline="PipeC"}   
#' \item or they (and all the genes they need) are embedded in a gene
#' \code{pipeline="PipeG"}.
#' }
#' 
#### HERE
#' In \code{xegaRun()}, 
#' by setting the option \code{pipeline="PipeC"} together with 
#' \code{replication} to one of "Kid1Pipeline", "Kid2Pipeline", or 
#' "DEPipeline", the selected replication function performs a set 
#' of random experiments to select the proper genes and based on their 
#' results compiles a function closure which embeds the genetic operator 
#' pipeline. These function closures are then executed in the evaluation  
#' step. This mechanism shifts the actual computation of all genetic 
#' operations but the selection of genes to the evaluation step.
#'   
#' The effect of genetic operator pipelines are
#' \enumerate{
#' \item moderate for seqential execution. The net effect depends on the relative cost 
#'       of the compilation process to the evaluation process. By compiling minimal genetic 
#'       operating pipelines, moderate savings are achieved. 
#' \item potentially high for parallel and distributed execution. The net effect depends on the 
#'       cost of the compilation process, the cost of communication and the cost of the evaluation process.
#'       The compilation process usually shifts more than 90 percent of the computational load to the 
#'       evaluation phase of the genetic algorithm (which can be parallelized).
#' }
#'
#' @section Distributed and Parallel Processing:
#'
#' In general, distributed and parallel processing requires a sequence of three steps:  
#' \enumerate{
#' \item Configure and start the distributed or parallel infrastructure.
#' \item Distribute processing and collect results. 
#'       In an evolutionary or genetic algorithm, the architectural pattern used for the implementation 
#'       of coarse-grained parallelism by parallel evaluation of the fitness of the genes of a population
#'       is the master/worker pattern. In principle, the \code{lapply()}-function for evaluating a population 
#'       of genes is replaced by a parallel version. 
#' \item Stop the distributed or parallel infrastructure.
#' }
#' 
#' For evolutionary and genetic algorithms, the second step is controlled by two parameters, 
#' namely \code{executionModel} and \code{uParApply}:
#' \enumerate{
#' \item If \code{uParApply=NULL}, then \code{executionModel} provides four ways of evaluating the 
#'       fitness of a population of genes:
#'       \enumerate{
#'       \item \code{executionModel="Sequential"}: The apply function used is \code{base::lapply()}. (Default).
#'       \item \code{executionModel="MultiCore"}:  The apply function used is \code{parallel::mclapply()}.
#'             If the number of cores is not specified by \code{cores}, the number of available cores 
#'             is determined by \code{parallelly::availableCores()}. 
#'       \item \code{executionModel="MultiCoreHet"}:  The apply function used is \code{parallel::mclapply()}
#'              with \code{mc.preschedule=FALSE}. 
#'             If the number of cores is not specified by \code{cores}, the number of available cores 
#'             is determined by \code{parallelly::availableCores()}. 
#'             This improves speed for tasks with a high variance in execution time.
#'       \item \code{executionModel="FutureApply"}:  The apply function used is \code{future.apply::future_lapply()}.
#'             The parallel/distributed model depends on a proper \code{future::plan()} statement. 
#'       \item \code{executionModel="Cluster"}:  The apply function used is \code{parallel::parLapply()}.
#'             The information about the configuration of the computing cluster (master, port, list of workers)
#'             must be provided by \code{Cluster=cl} where 
#'             \code{cl<-parallel::makeClusterPSOCK( rep(localhost, 5))}
#'             generates the cluster object and starts the R processes (of 5 workers in the same machine).  
#'       }
#' \item Assume that a user-defined parallel apply function has been defined and called \code{UPARAPPLY}. 
#'       By setting \code{uParApply=UPARAPPLY}, the \code{lapply()} function used is \code{UPARAPPLY()}. 
#'       This overrides the specification by \code{executionModel}. For example,
#'       parallelization via the MPI interface can be achieved by providing a user-defined parallel 
#'       \code{lapply()} function which is implemented by a user-defined function whose function body 
#'       is the line \code{Rmpi::mpi.parLapply( pop, FUN=EvalGene, lF=lF)}.
#' }
#'
#' See package \code{xegaPopulation}  <https://CRAN.R-project.org/package=xegaPopulation> 
#' 
#' \strong{Acknowledgment.}The author acknowledges support by the state of Baden-Württemberg through bwHPC.
#'
#' @section Reporting:
#'
#' \itemize{ 
#'  \item \code{verbose} controls the information reported on the screen. 
#'  If \code{verbose} is \code{1}, then one dot is printed per generation to the console.
#'  \item \code{reportEvalErrors=TRUE} reports the output of errors of fitness function evaluations 
#'        to the console. Grammatical evolution (algorithm "sge") routinely attempts to evaluate 
#'        incomplete derivation trees. This leads to an evaluation error of the fitness function. 
#'  \item \code{profile=TRUE} measures the time spent in executing the main blocks of the algorithm:
#'        \code{InitPopulation()}, \code{NextPopulation()}, \code{EvalPopulation()}, 
#'        \code{ObservePopulation()}, and \code{SummaryPopulation()}. The measurements are stored in the
#'        named list \code{$timer} of the result object. 
#'  \item \code{allSolutions=TRUE} collects all solutions with the same fitness value. 
#'        The lists of the genotypes and phenotypes of these solutions are stored 
#'        in \code{$solution$allgenotypes} and \code{$allphenotypes} of the result object of the algorithm.
#'  \item \code{batch=TRUE} writes the result object and \code{logevals=TRUE} writes a list of all evaluated genes
#'        in an \code{rds}-file in the current directory. \code{path} allows to write the \code{rds}-files  
#'        into another directory. The existence of the directory specified by \code{path} is not checked.
#'        \code{batch=TRUE} combined with \code{verbose=TRUE} should be used in batch environments on 
#'        HPC environments.
#'  \item \code{anytime=TRUE} writes the result object \code{path/xegaAnyTimeResult.rds} after each generation. 
#'        Only the most recent result is available.
#'  }
#'
#' @section Semantics of the local function list lF:
#'
#' This is experimental. The rationale is to 
#' save on communication cost in multi-core processing.
#' \itemize{
#' \item byValue is the Default.
#' \item byReference converts lF to an evironment.
#' } 
#'
### Problem Specification
#'
#' @param penv        Problem environment.
#'
#' @param grammar     A compiled grammar object. Default: NULL.
#'                    Example: \code{compileBNF(booleanGrammar())}              
#'
#' @param max         If \code{TRUE} then Maximize! Default: TRUE.
#'                    Used in functions \code{EvalGeneDet}, \code{EvalGeneStoch},
#'                    \code{EvalGeneU}, and \code{EvalGeneR} 
#'                    of package \code{xegaSelectGene}.
#'
### Algorithm 
#'
#' @param algorithm   Specifies the algorithm class dependent 
#'                    on gene representation:
#'                    \itemize{
#'                    \item "sga": Binary representation (Default).
#'                    \item "sgde": Real representation. 
#'                           E.g. Differential evolution.
#'                    \item "sgperm": Permutation representation.
#'                    \item "sge": Binary representation. 
#'                                 Grammatical evolution.    
#'                                 (Not yet variable length.)
#'                    \item "sgede": Real representation. 
#'                          Genetic operators from differential evolution.
#'                             Grammatical evolution.    
#'                                 (Not yet variable length.)
#'                    \item "sgp": Derivation tree representation. 
#'                                 Grammar Based Genetic Programming.
#'                    }
#'
### Basic Parameters
#'
#' @param popsize     Population size. Default: 100.
#' @param generations Number of generations. Default: 20.
#' @param crossrate   Probability of applying crossover operator. Default: 0.20.
#'                    (Global parameter)
#' @param mutrate     Probability of applying mutation operator. Default: 1.0.
#'                    (Global parameter)
#'
#' @param elitist     Boolean. If \code{TRUE}, 
#'                    then keep the best solution in the population.
#'                    Default: \code{TRUE}.
#' @param replay      Integer. If \code{replay>0}, then use \code{replay} 
#'                        as the seed of the random number generator and  
#'                        store it for the exact repetition of this run.
#'                    Default: 0.
### End of Basic Parameters
#' @param maxdepth    The maximal depth of a derivation tree. Default: 7. (\code{"sgp"}).
#' @param maxtrials   Maximal number of trials for finding subtrees with the same root symbol.
#'                    Default: 5. (\code{sgp}).
#'
#' @param codons      The maximal number of codons of derivations on a gene. 
#'                    Default: 25. (\code{"sge"}).
#' @param codonBits   The number of bits of a codon.
#'                    Default: 0. (\code{"sge"}).
#' @param codonPrecision Specify the method to set the number of bits of a
#'                    codon (\code{"sge"}):  
#'                    \itemize{
#'                    \item "Min": Sufficient to code the maximal number 
#'                                 of choices of production rules for 
#'                                 a non-terminal.
#'                    \item "LCM": Contains the least common multiple 
#'                                 of the prime factors of the number of 
#'                                 choices of production rules for all 
#'                                 non-terminals.
#'                    \item "MaxPBias": The computed precision guarantees
#'                                 that the choice rule bias for a non-terminal
#'                                 is below \code{maxPBias}. 
#'                    }
#'          Argument of function factory 
#'          \code{xegaGePrecisionFactory} in package \code{xegaGeGene}.
#' @param maxPBias    The threshold of the choice rule bias. 
#'                    Default: \code{0.01}. (\code{"sge"}).
#'
#' @param evalmethod  Specifies the method of function evaluation:
#'          \itemize{ 
#'          \item  "EvalGeneU": The function is always evaluated. (Default)
#'          \item  "EvalGeneR": The function is always evaluated. 
#'                              Repairs of the gene by the decoder are 
#'                              possible.
#'          \item  "Deterministic": The function is evaluated only once.
#'          \item  "Stochastic": The expected function value and its 
#'                                  variance are incrementally updated.
#'      }
#'          Argument of function factory 
#'          \code{EvalGeneFactory} in package xegaSelectGene.
#'     
#' @param evalrep           Specifies the number of repeated fitness 
#'                          evaluations of a (stochastic) function. 
#' @param reportEvalErrors  Report errors in the evaluation 
#'                          of fitness functions. Default: TRUE.
#'
### Start of genemap
#' @param genemap     Gene map for decoding. Default: "Bin2Dec".
#'                    The default value works only for algorithm "sga".
#'                    Used as \code{method} argument of the function factory
#'                    \code{sgXGeneMapFactory} of package \code{xega}.
#'
#'                    Available options determined by 
#'                    \code{algorithm}:
#'                    \itemize{
#'                    \item "sga": Binary representation (Default).
#'                    \itemize{
#'                    \item "Bin2Dec": For real parameter vectors. 
#'                    \item "Gray2Dec": For real parameter vectors.
#'                    \item "Identity": For 0/1 parameter vectors.
#'                    \item "Permutation": For permutations.
#'                    }
#'                    See the function factory 
#'                    \code{xegaGaGeneMapFactory} in package \code{xegaGaGene}.
#'                    \item "sgp": Derivation tree. 
#'                           Gene map is not used, but must be specified.
#'                           We use \code{xegaGaGene::xegaGaGeneMapFactory} 
#'                           with \code{method="Identity"}.
#'                    \item "sge": Binary representation (Default).
#'                          How are genes decoded?
#'                    \itemize{
#'                    \item "Mod": The modulo rule.
#'                    \item "Bucket": The bucket rule (with the mLCM). 
#'                          Problem: Mapping \code{1: 2^k} to \code{1:mLCMG}.
#'                    }
#'                    See the function factory 
#'                    \code{xegaGeGeneMapFactory} in package \code{xegaGeGene}.
#'                    \item "sgde": Real coded gene.
#'                           We use \code{xegaDfGene::xegaDfGeneMapFactory} 
#'                           with \code{method="Identity"}.
#'                           Function used: \code{xegaDfGene::xegaDfGeneMapIdentity}
#'                    \item "sgperm": Permutation gene.
#'                           Gene map is not used, but must be specified.
#'                           We use \code{xegaDfGene::xegaDfGeneMapFactory} 
#'                           with \code{method="Identity"}.
#'                           Function used: \code{xegaDfGene::xegaDfGeneMapIdentity}
#'                    } 
### End of genemap
#' @param decoder     Specifies a decoder for a gene, Default: \code{"DecodeGene"}.
#'                    For algorithm \code{sge}, a second decoder is available:
#'                    \code{DecodeGeneDT}. This decoder is faster, but it may generate code which
#'                    still contains non-terminal symbols and which does not work.
#'
#' @param crossrate2  Crossover rate for genes with below 
#'                    ``average'' fitness. 
#'                    Probability of applying crossover operator 
#'                    for genes with a ``below average'' fitness.
#'                    Default: 0.30. 
#'                    (Global parameter)
#'
#' @param ivcrossrate Specifies the method of determining the crossover rate.
#'                    \itemize{
#'                    \item 
#'                     "Const" Constant crossover rate. 
#'                     The probability of applying the crossover operator
#'                     is constant for the whole run of the algorithm.
#'                     Default: "Const".
#'                     \item "IV" Individually variable crossover rate.
#'                     The crossrate of a gene is determined by the following threshold
#'                     rule: 
#'                     If the fitness of the gene is higher than 
#'                     \code{lF$CutoffFit()*} \code{lF$CBestFitness()}, then 
#'                     \code{lF$CrossRate1()} else \code{lF$CrossRate2()}
#'                     is used.
#'                     } 
#'          Argument of function factory 
#'          \code{CrossRateFactory} in package \code{xegaPopulation}.
#'
#' @param uCrossSwap  The fraction of positions swapped in the
#'                    parametrized uniform crossover operator.
#'                    A local crossover parameter.
#'                    Default: 0.2. (\code{"sga"} and \code{"sgde"}). 
#'                    Used in packages \code{xegaGaGene} and \code{xegaDfGene}
#'                    for functions 
#'                    \code{xegaGaUPCross2Gene},
#'                    \code{xegaDfUPCross2Gene},
#'                    \code{xegaGaUPCrossGene}, and
#'                    \code{xegaDfUPCrossGene}.    
#'
#' @param mincrossdepth  minimal depth of exchange nodes (roots of subtrees
#'                       swapped by crossover). (\code{"sgp"}).
#' @param maxcrossdepth  Maximal depth of exchange nodes (roots of subtrees
#'                       swapped by crossover). (\code{"sgp"}).
#'                     Used in package \code{xegaGpGene} functions 
#'                      \code{xegaGpCrossGene} and \code{xegaGpCross2Gene}
#'                     in package xegaGpGene. 
#'
#' @param crossover   Crossover method. Default: "Cross2Gene".
#'                    The choice of crossover methods depends on the 
#'                    setting of the argument \code{algorithm}.
#'                    Used as the \code{method} argument in function factory
#'                    \code{sgXCrossoverFactory} of package \code{xega}.
#'
#'                    \itemize{
#'                    \item \code{algorithm="sga"}:
#'                    \code{crossover} is an argument of function factory 
#'                    \code{xegaGaCrossoverFactory} in package \code{xegaGaGene}.
#'                    \itemize{
#'                    \item Crossover operators with 1 kid:
#'                    \itemize{
#'                    \item "CrossGene"  one-point crossover. 
#'                    \item "UCrossGene" uniform crossover.
#'                    \item "UPCrossgene" parameterized uniform crossover.
#'                          Local parameter: \code{uCrossSwap}.
#'                    }
#'                    \item Crossover operators with 2 kids:
#'                    \itemize{
#'                    \item "Cross2Gene"  one-point crossover. 
#'                    \item "UCross2Gene" uniform crossover.
#'                    \item "UPCross2gene" parameterized uniform crossover.
#'                          Local parameter: \code{uCrossSwap}.
#'                    }
#'                    }
#'                    \item \code{algorithm="sgp"}:
#'                    \code{crossover} is an argument of function factory 
#'                    \code{xegaGpCrossoverFactory} in package \code{xegaGpGene}.
#'                    \itemize{
#'                    \item Crossover operators with 1 kid:
#'                    \itemize{
#'                    \item "AllCrossGene"  position-based one-point crossover. 
#'                    }
#'                    \item Crossover operators with 2 kids:
#'                    \itemize{
#'                    \item "Cross2Gene" position-based one-point crossover. 
#'                    }
#'                    }
#'                    \item \code{algorithm="sge"}:
#'                    We use the factory \code{xegaGaCrossoverFactory}.
#'
#'                    (Adaptation needed for variable-length binary
#'                    representation.)
#'
#'                    \item \code{algorithm="sgde"}:
#'                    \code{crossover} is an argument of function factory 
#'                    \code{xegaDfCrossoverFactory} in package \code{xegaDfGene}.
#'                    \itemize{
#'                    \item Crossover operators with 1 kid:
#'                    \itemize{
#'                    \item "CrossGene"  one-point crossover  (of reals)
#'                    \item "UCrossGene" uniform crossover  (of reals)
#'                    \item "UPCrossGene" parametrized 
#'                           uniform crossover  (of reals).
#'                           Local parameter: \code{uCrossSwap}.
#'                    }
#'                    \item Crossover operators with 2 kids: Not implemented.
#'                    }
#'
#'                    \item \code{algorithm="sgperm"}:
#'                    \code{crossover} is an argument of function factory 
#'                    \code{xegaPermCrossoverFactory} in package \code{xegaPermGene}.
#'                    \itemize{
#'                    \item Crossover operators with 1 kid:
#'                    \itemize{
#'                    \item "CrossGene"  position-based one-point crossover. 
#'                    }
#'                    \item Crossover operators with 2 kids:
#'                    \itemize{
#'                    \item "Cross2Gene" position-based one-point crossover. 
#'                    }
#'                    }
#'                    }
#'    
#' @param mutrate2    Mutation rate. Default: 1.0.
#'                    (Global parameter).
#'
#' @param ivmutrate "Const" or "IV" (individually variable). 
#'                     Default: "Const".
#'
#' @param bitmutrate     Bit mutation rate. Default: 0.005.
#'                    A local mutation parameter. (\code{"sga"} and \code{"sge"}).
#'                     Used in package \code{xegaGaGene} functions 
#'                      \code{MutateGene}
#'                      \code{IVAdaptiveMutateGene}
#'
#' @param bitmutrate2    Bit mutation rate for genes
#'                       with ``below average'' fitness. Default: 0.01.
#'                    A local mutation parameter. (\code{"sga"} and \code{"sge"}).
#'                     Used in package \code{xegaGaGene} functions 
#'                      \code{IVAdaptiveMutateGene}
#' 
#' @param maxmutdepth   Maximal depth of a derivation tree inserted 
#'                      by a mutation operation. Default: 3. (\code{"sgp"}).
#' @param minmutinsertiondepth   Minimal depth at which an insertion tree
#'                      is inserted. Default: 1. (\code{"sgp"}).
#' @param maxmutinsertiondepth   Maximal depth at which an insertion tree
#'                      is inserted. Default: 7. (\code{"sgp"}).
#'                     Used in package \code{xegaGpGene} function
#'                      \code{xegaGpMutateGene}.
#'
#' @param lambda        Decay rate. Default: \code{0.05}.
#'                    A local mutation parameter. (\code{"sgperm"}).
#'                     Used in package \code{xegaPermGene} function
#'                      \code{xegaPermMutateGenekInversion}.
#' 
#' @param max2opt     Maximal number of trials to find  
#'                    an improvement by a random edge exchange 
#'                    in a permutation. Default: \code{100}. (\code{"sgperm"}).
#'                     Used in package \code{xegaPermGene} function
#'                      \code{xegaPermMutateGene2Opt}
#'                    and  \code{xegaPermMutateGeneOptLK}.
#'
#' @param scalefactor1  Scale factor for differential mutation operator 
#'                      (Default: \code{0.9}). (\code{"sgde"}).
#' @param scalefactor2  Scale factor for differential mutation operator 
#'                      (Default: \code{0.2}). (\code{"sgde"}).
#' @param scalefactor   Method for setting scale factor (\code{"sgde"}):
#'                      \itemize{
#'                      \item "Const":  Constant scale factor configured by
#'                             \code{scalefactor1}.
#'                      \item "Uniform": A random scale factor in the interval 
#'                             from \code{0.000001} to \code{1.0}.
#'                      \item "DERSF": A random scale factor in the interval 
#'                             from \code{0.5} to \code{1.0}.
#'                      \item "DETVSF": The scale factor is linear 
#'                            decaying from an upper bound 
#'                            (\code{scalefactor1==0.9}) to a lower bound 
#'                            (\code{scalefactor2==0.2}) with the 
#'                            number of \code{generations}.
#'                      \item "CauchySF": Bounded 
#'                            Cauchy distributed scale factor 
#'                            with a scale parameter which increases
#'                            with the number of \code{generations}.
#'                      \item "FBSASF": Fitness based self adaptive 
#'                            scale factor.
#'                      \item "RGSF": Random Gaussian 
#'                            scale factor (Random pick of a random 
#'                            number from either  
#'                            \code{abs(rnorm(1, 0.3, 0.3))} or
#'                            \code{abs(rnorm(1, 0.7, 0.3))}.
#'                       }
#' @param cutoffFit   Cutoff for fitness.      Default: \code{0.5}. 
#'                    (\code{"sga"} and \code{"sge"}).
#'                     Used in package \code{xegaGaGene} function
#'                      \code{IVAdaptiveMutateGene}.
#'
#' @param mutation    Label specifies the mutation method
#'                    dependent on \code{algorithm}. Default: "MutateGene".
#'                    The (global) probability of calling a mutation method
#'                    is specified by \code{mutrate} and \code{mutrate2}.
#'                    Used as \code{method} argument of the function factory 
#'                    \code{sgXMutationFactory} in package \code{xega}. 
#'       
#'                    \itemize{
#'                    \item \code{algorithm="sga"}:
#'                    \code{mutation} is an argument of function factory 
#'                    \code{xegaGaMutationFactory} in package \code{xegaGaGene}.
#'                    \itemize{
#'                    \item "MutateGene": Bitwise mutation. 
#'                          Local parameter: \code{bitmutrate}.
#'                          Function used: \code{xegaGaGene::xegaGaMutateGene}.
#'                    \item "IVM": Individually variable mutation.
#'                          Intuitively, we know that 
#'                          bad genes need higher mutation rates.
#'                          Good genes have a fitness which is 
#'                          above a threshold fitness. The threshold
#'                          is determined as a percentage of the 
#'                          current best fitness in the population.
#'                          The percentage is set by the parameter 
#'                          \code{cutoffFit}. 
#'                          Local parameters: \code{bitmutrate} for good genes.
#'                          \code{bitmutrate2} for bad genes.
#'                          \code{bitmutrate2} should be higher than 
#'                          \code{bitmutrate}.
#'                    }
#'                    \item \code{algorithm="sgp"}:
#'                    \code{mutation} is an argument of function factory 
#'                    \code{xegaGpMutationFactory} in package \code{xegaGpGene}.
#'
#'                    \itemize{
#'                    \item "MutateGene": Random insertion of 
#'                           a random derivation tree. 
#'                          Local parameter: \code{maxmutdepth}.
#'                          Function used: \code{xegaGpGene::xegaGpMutateGene}.
#'                     }
#'
#'                    \item \code{algorithm="sge"}:
#'                    \code{mutation} is an  argument of function factory 
#'                    \code{xegaGaMutationFactory}.
#'                    Nothing specific to grammatical evolution has been implemented.
#'
#'                    \item \code{algorithm="sgde"}:
#'                    \code{mutation} is an  argument of function factory 
#'                    \code{xegaDfMutationFactory} in package \code{xegaDfGene}.
#'
#'                    \itemize{
#'                    \item "MutateGene": Add the scaled difference 
#'                          of the parameters of two randomly selected
#'                          to a gene.
#'                          Local parameters: Choice of function for 
#'                                      \code{scalefactor} as well as
#'                                            \code{scalefactor1}  
#'                                            and \code{scalefactor2}.
#'                          Function used: \code{xegaDfGene::xegaDfMutateGeneDE}.
#'                     }
#'
#'                    \item \code{algorithm="sgperm"}:
#'                    \code{mutation} is an  argument of function factory 
#'                    \code{xegaPermMutationFactory} in package \code{xegaPermGene}.
#'
#'        \itemize{
#'        \item "MutateGene": 
#'              Function used: \code{xegaPermGene::xegaPermMutateGeneOrderBased}.
#'        \item "MutateGeneOrderBased": See "MutateGene". 
#'        \item "MutateGenekInversion": 
#'              Function used: \code{xegaPermGene::xegaPermMutateGenekInversion}.
#'        \item "MutateGene2Opt": 
#'              Function used: \code{xegaPermGene::xegaPermMutateGene2Opt}.
#'        \item "MutateGenekOptLK": 
#'              Function used: \code{xegaPermGene::xegaPermMutateGenekOptLK}.
#'        \item "MutateGeneGreedy": 
#'              Function used: \code{xegaPermGene::xegaPermMutateGeneGreedy}.
#'        \item "MutateGeneBestGreedy": 
#'              Function used: \code{xegaPermGene::xegaPermMutateGeneBestGreedy}.
#'        \item "MutateGeneMix": 
#'              Function used: \code{xegaPermGene::xegaPermMutateMix}.
#'        }
#'                    } 
#'
#' @param replication "Kid1", "Kid1Pipeline", "Kid1PipelineG", "Kid2", "Kid2Pipeline" or 
#'                    "Kid2PipelineG". Default: "Kid1".
#'                    For algorithms "sga", "sgPerm", "sgp", and "sge":
#'                    "Kid1" means a crossover operator with one kid.
#'                    "Kid1Pipeline" means a function closure 
#'                     with a genetic operator pipeline is returned.
#'                    "Kid1PipelineG" means a gene 
#'                     with a genetic operator pipeline embdded is returned.
#'                    "Kid2" means a crossover operator with two kids.
#'                    "Kid2Pipeline" means a function closure 
#'                     with a genetic operator pipeline is returned.
#'                    "Kid2PipelineG" means a gene
#'                     with a genetic operator pipeline embedded is returned.
#'                     
#'                     For algorithms "sgde" and "sgede", 
#'                     \code{replication} must be 
#'                     set to "DE", "DEPipeline", or "DEPipelineG". 
#'
#'                     The pipeline versions of replication 
#'                     requires to set \code{pipeline="PipeC"} or \code{pipeline="PipeG"} too.
#'
#'                    The pipeline versions of replication 
#'                    generate either a genetic operator pipeline 
#'                    as a function closure or a genetic operator pipeline embedded in a gene.
#'                    We call this transformation pipeline compilation.
#'                    The execution of the function closures or of the gene embedded pipelines
#'                    is shifted to the evaluation step and, thus, can 
#'                    be parallelized.
#'
#'                    When genetic operator pipelines are used,
#'                    the gene representation of the  population vector cycles 
#'                    \itemize{
#'                    \item either between function closures
#'                          and named lists as elements (\code{pipeline="PipeC"})
#'                    \item or between genes extended with a list element \code{$Pipeline()} 
#'                          with the genetic operator pipeline and the needed additional 
#'                          genes (1 for crossover, 3 for differential evolution)
#'                          and a normal gene without these elements
#'                          (\code{pipeline="PipeG"}).
#'                    }
#'
#'                    Used as the \code{method} argument of the 
#'                    function factory \code{sgXReplicationFactory} 
#'                    of package \code{xega}.
#'
#####
#' @param initgene    Default: "InitGene".
#'                    For algorithm "sgp", 
#'                    \enumerate{
#'                    \item "InitGene": Random derivation tree. 
#'                    \item "InitGeneGe": Random derivation tree from 
#'                                        random integer vector.
#'                    }
#'
#' @param offset  Offset used in proportional selection. Default: 1. 
#'            Used in the following functions of package \code{xegaSelectGene}: 
#'            \code{ScaleFitness},
#'            \code{PropFitOnLn},
#'            \code{PropFit},
#'            \code{PropFitM},
#'            \code{PropFitDiffOnLn},
#'            \code{PropFitDiff},
#'            \code{SUS}.
#'
##            \code{\link[xegaSelectGene:ScaleFitness]{ScaleFitness}},
##            \code{\link[xegaSelectGene:PropFitOnLn]{PropFitOnLn}},
##            \code{\link[xegaSelectGene:PropFit]{PropFit}},
##            \code{\link[xegaSelectGene:PropFitM]{PropFitM}},
##            \code{\link[xegaSelectGene:PropFitDiffOnLn]{PropFitDiffOnLn}},
##            \code{\link[xegaSelectGene:PropFitDiff]{PropFitDiff}},
##            \code{\link[xegaSelectGene:SUS]{SUS}}.
#'
#' @param eps         Epsilon in proportional 
#'                    fitness difference selection. Default: 0.01.
#'                    Used in package \code{xegaSelectGene} function
#'                    \code{PropFitDiffM}.
#'
##                    \code{\link[xegaSelectGene:PropFitDiffM]{PropFitDiffM}}.
#'
#' @param scaling     Scaling method. Default: "NoScaling".
#'                    Available scaling methods: 
#'                    \itemize{
#'                    \item "NoScaling", 
#'                    \item "ConstantScaling" (Static), 
#'                    \item "ThresholdScaling" (Dynamic), 
#'                    \item "ContinuousScaling" (Dynamic).
#'                    }
#'                    Argument of function factory 
#'                    \code{ScalingFactory} in package \code{xegaSelectGene}.
#'
#' @param scalingExp  Scaling exponent \code{k} in \code{fit^k}.
#'                    With "ConstantScaling": 0 =< k. 
#'                    With "ThresholdScaling": 1 < k. (Default: 1)
#'                    Used in package \code{xegaSelectGene}, functions
#'             \code{ScalingFitness},
#'             \code{ThresholdScaleFitness}.
#'
##             \code{\link[xegaSelectGene:ScalingFitness]{ScalingFitness}},
##             \code{\link[xegaSelectGene:ThresholdScaleFitness]{ThresholdScaleFitness}}.
#'
#' @param scalingExp2 Scaling exponent 
#'                    for "ThresholdScaling": \code{0 <= k <1}. (Default: \code{1})
#'                    Used in package \code{xegaSelectGene} function
#'             \code{ThresholdScaleFitness}.
#'
##             \code{\link[xegaSelectGene:ThresholdScaleFitness]{ThresholdScaleFitness}}.
#'
#' @param scalingThreshold Numerical constant. Default: \code{0.0}.
#'                    If the ratio of dispersion measures is in 
#'                    [\code{(1-scalingThreshold)}, \code{1+scalingThreshold)}], 
#'                    fitness is not scaled.
#'                    Used in package \code{xegaSelectGene} function
#'             \code{ThresholdScaleFitness}.
#'
##             \code{\link[xegaSelectGene:ThresholdScaleFitness]{ThresholdScaleFitness}}.
#'
#' @param rdmWeight   Numerical constant. Default: \code{1.0}. Weight of 
#'                    ratio of dispersion measures in continuous scaling.
#'                    Used in package \code{xegaSelectGene} function
#'             \code{ContinuousScaleFitness}.
#'
##             \code{\link[xegaSelectGene:ContinuousScaleFitness]{ContinuousScaleFitness}}.
#'
#' @param drMin       Minimal allowable dispersion ratio. Default: \code{0.5}.
#'                    Used in package \code{xegaSelectGene} function
#'             \code{DispersionRatio}.
##             \code{\link[xegaSelectGene:DispersionRatio]{DispersionRatio}}.
#'
#' @param drMax       Maximal allowable dispersion ratio. Default: \code{2.0}.
#'                    Used in package \code{xegaSelectGene} function
#'             \code{DispersionRatio}.
##             \code{\link[xegaSelectGene:DispersionRatio]{DispersionRatio}}.
#'
#' @param dispersionMeasure  Dispersion measure specifies a concrete dispersion measure
#'                           of the population's fitness vector at generation \code{k}.
#'                           (e.g. the variance of the population fitness).
#'                    In dynamic scaling methods the ratio of dispersion measures at \code{k}
#'                    and \code{k-j} is often used to adapt the selection pressure.
#'                    Default: "var".
#'                    Available dispersion measures: 
#'                    "var, "std", "mad", "cv", "range", "iqr".
#'                    Argument of function factory 
#'                    \code{DispersionMeasureFactory} in package \code{xegaSelectGene}.
#'
#' @param scalingDelay The ratio of dispersion measures compares the current
#'                     population dispersion at t with the population dispersion 
#'                     at t-scalingdelay. Default: \code{1}.
#'                    Used in package \code{xegaSelectGene} function
#'             \code{DispersionRatio}.
##             \code{\link[xegaSelectGene:DispersionRatio]{DispersionRatio}}.
#'
#' @param tournamentSize   Tournament size. Default: 2. 
#'                    Used in package \code{xegaSelectGene} functions
#'                    \code{SelectTournament},
#'                    \code{SelectSTournament}.
##                    \code{\link[xegaSelectGene:SelectTournament]{SelectTournament}},
##                    \code{\link[xegaSelectGene:SelectSTournament]{SelectSTournament}}.
#'
#' @param selectionBias   (> 1.0). Controls selection pressure for 
#'        Whitley's linear rank selection
#'        with selective pressure. Default: 1.5. Near 1.0: almost
#'        uniform selection.
#'                    Used in package \code{xegaSelectGene} function
#'                    \code{SelectLRSelective},
#'
#' @param maxTSR    Controls selection pressure for 
#'                  Grefenstette and Baker's linear rank selection 
#'                  method. Should be higher than 1.0 and lower equal 2.0.
#'                  Default: 1.5.
#'                    Used in package \code{xegaSelectGene} function
#'         \code{SelectLinearRankTSR}.
##         \code{\link[xegaSelectGene:SelectLinearRankTSR]{SelectLinearRankTSR}},
#'
#' @param selection      Selection method for the first parent of crossover. 
#'                       Default: "SUS". 
#' @param mateselection  Selection method for the second parent of crossover. 
#'                       Default: "SUS". 
#'
#' Available selection methods for the selection method of a parent:
#' \itemize{
#' \item Uniform random selection: "Uniform".
#' \item Uniform random selection without replacement: "UniformP".
#' \item Proportional to fitness: 
#'       "ProportionalOnln" (fastest), "Proportional", "ProportionalM",
#' \item Proportional to fitness differences: 
#'       "PropFitDiffOnln" (fastest), "PropfitDiff", "PropfitDiffM",
#' \item Stochastic universal sampling: "SUS", 
#' \item Tournament selection: "Duel" (fastest), "Tournament", "STournament",  
#' \item Rank selection: "LRSelective" (fastest), "LRTSR".
#' }
#'                    Argument of function factory 
#'                    \code{SelectGeneFactory} in package \code{xegaSelectGene}.
#'
#' @param selectionContinuation  Boolean. If \code{TRUE}, 
#'        precomputes selection indices for next generation once and
#'        transforms selection function to index lookup continuation.
#'        Default: \code{TRUE}.
#'        Used in package \code{xegaPopulation} function \code{xegaNextPopulation}.
#'
#' @param accept   Acceptance rule for a new gene. Default: "All".
#'        \itemize{
#'          \item "All"  function \code{AcceptNewGene} 
#'          \item "Best"  function \code{AcceptBest} 
#'          \item "Metropolis" function \code{AcceptMetropolis}.
#'                The behavior of this acceptance rule depends on:
#'                \enumerate{
#'                \item The distance between the fitness values.
#'                      The larger the distance, the larger the drop 
#'                      in acceptance probability.
#'                \item \code{alpha} is \code{1} minus the discount rate of the cooling
#'                      schedule. \code{alpha} is in \code{[0, 1]}.
#'                      The smaller the \code{alpha}, the faster the drop 
#'                      in temperature and thus acceptance probability.
#'                \item \code{beta} a constant. The larger the \code{beta},
#'                      the faster the drop in acceptance probability.
#'                \item \code{temperature} the starting value of the 
#'                      temperature. Must be higher than the number of 
#'                      generations.
#'                }
#'          \item "IVMetropolis" function \code{AcceptIVMetropolis}.
#'                The behavior of this acceptance rule is qualitatively the same as that 
#'                of the Metropolis acceptance rule above.
#'                The acceptance rule is adaptive by a correction of the temperature
#'                in proportion to the difference between the fitness of the current best and
#'                the fitness of the gene considered.
#'                }
#'                    Argument of function factory 
#'                    \code{AcceptFactory} in package \code{xegaPopulation}.
#'
#' @param alpha    \code{1} minus the  discount rate for temperature. (Default: \code{0.99}).
#'                    (Used in the cooling schedule at the end of main GA-loop.)
#' 
#' @param beta     Constant in the Metropolis acceptance rule. (Default: \code{2.0}).
#'                    (Used in the Metropolis acceptance rule.)
#'
#' @param temp0    Starting value of temperature (Default: \code{40}).
#'                    (Used in the Metropolis acceptance rule. Updated in the cooling schedule.)
#'
#' @param tempN    Final value of temperature (Default: \code{0.01}).
#'                    (Used in the Metropolis acceptance rule. Updated in the cooling schedule.)
#'
#' @param cooling  Cooling schedule for temperature. (Default: "ExponentialMultiplicative")
#'                 \itemize{
#'                 \item "ExponentialMultiplicative" calls \code{ExponentialMultiplicativeCooling}
#'                 \item "LogarithmicMultiplicative" calls \code{LogarithmicMultiplicativeCooling}
#'                 \item "PowerMultiplicative" calls \code{PowerMultiplicativeCooling}
#'                 \item "PowerAdditive" calls \code{PowerAdditiveCooling}
#'                 \item "ExponentialAdditive" calls \code{ExponentialAdditiveCooling}
#'                 \item "TrigonometricAdditive" calls \code{TrigonometricAdditiveCooling}
#'                 }
#'                    Argument of function factory 
#'                    \code{CoolingFactory} in package \code{xegaPopulation}.
#'
#' @param coolingPower  Exponent for PowerMultiplicative cooling schedule. 
#'                     (Default: 1. This is called linear multiplicative cooling.)
#'
#' @param verbose  
#'        The value of \code{verbose} (Default: 1) controls the
#'              information displayed:
#'              \enumerate{
#'              \item \code{== 0}: Nothing is displayed.
#'
#'              \item \code{== 1}: 1 point per generation.
#'
#'              \item \code{> 1}: Max(fit), number of solutions, indices.
#'
#'              \item \code{> 2}: and population fitness statistics.
#'
#'              \item \code{> 3}: and fitness, value of phenotype, 
#'                                  and phenotype.
#'              \item \code{> 4}: and str(genotype). 
#'              }
#'
#'
#' @param logevals  Boolean.
#'        If \code{TRUE} then log all evaluations and their parameters 
#'        in the file
#'        \code{xegaEvalLog<exclusive pattern>.rds}. Default: \code{FALSE}.
#'        
#'        \code{log<-readRDS(xegaEvalLog<exclusive pattern>.rds)} reads the log.
#'        The \code{log} is a list of named lists with the following elements:
#'         \itemize{
#'         \item \code{$generation}:   The generation.
#'         \item \code{$fit}:          The fitness value.
#'         \item \code{$sigma}:        The standard deviation of the 
#'                                     fitness value, if it exists.
#'                                     Default: \code{0}.
#'         \item \code{$obs}:          The number of observations for 
#'                                     computing the 
#'                                     fitness value, if it exists.
#'                                     Default: \code{0}.
#'         \item \code{$phenotype}:    The phenotype of the gene.
#'         }
#'
#' @param allsolutions  Boolean. If \code{TRUE}, then return all the best solutions.
#'        Default: \code{FALSE}.
#'
#' @param early     Boolean. If \code{FALSE} (Default), ignore the code for 
#'                  early termination. 
#'                  See \link{Parabola2DEarly}.
#' @param terminationCondition  Termination condition.
#'                  Avalailable:
#'                  \itemize{
#'                  \item "NoTermination" (Default).
#'                  \item "AbsoluteError": 
#'                        Algorithm ends if current optimum is in optimum \code{+/-} \code{terminationEps}. 
#'                  \item "RelativeError": 
#'                       Algorithm ends if current optimum is in optimum \code{+/-} \code{terminationEps*optimum}. 
#'                       If the optimum is \code{0}, the interval has length \code{0}. 
#'                  \item "RelativeErrorZero": 
#'                       Algorithm ends if current optimum is in optimum \code{+/-} \code{terminationEps*optimum}. 
#'                       If the optimum is \code{0}, the interval is from \code{-terminationEps} to \code{terminationEps}.
#'                  \item "PAC": 
#'                       Algorithm ends if current optimum is in ub \code{+/-} \code{terminationEps*optimum}. 
#'                       If ub is \code{0}, the interval is from \code{-terminationEps} to \code{terminationEps}.
#'   ub is an estimated upper PAC bound for the global optimum. 
#'   The probability that the optimum is above ub is set by \code{PACdelta}.
#'   The epsilon environment by \code{terminationEps}.
#'                  \item "GEQ": Algorithm ends if the current optimal phenotype value is greater or equal 
#'                               than \code{terminationThreshold}. 
#'                  \item "LEQ": Algorithm ends if the current optimal phenotype value is less or equal 
#'                               than \code{terminationThreshold}. 
#'                  } 
#'                            
#' @param terminationEps  Fraction of the known optimal solution
#'                     for computing termination interval. Default: \code{0.01}.
#'                  See \link{Parabola2DEarly}.
#' @param terminationThreshold   A threshold for terminating the algorithm. Defaul: \code{0.0}.
#' @param worstFitness  Set the worst fitness. Default: \code{0.0}.
#'                   Used e.g. in \code{evalgeneU()} for giving genes whose evaluation failed a very low fitness value
#'                   to decrease their survival rate into the next generation. 
#' @param PACdelta   \code{P(ub<opt)<PACdelta}. Default: \code{0.01}. 
#' @param fSpace    Function space of fitness function. Default: "Hilbert". 
#'
#' @param cores   Number of cores used for multi-core parallel execution.
#'                (Default: \code{NA}. \code{NA} means that the number of cores 
#'                is set by \code{parallelly:availableCores()} 
#'                if the execution model is "MultiCore" or "MultiCoreHet".
#'                  
#' @param pipeline Method of pipeline compilation. 
#'                 Default: "NoPipe".
#'               \itemize{
#'               \item "NoPipe": No pipeline compilation.
#'               \item "PipeC": Pipeline compilation generates  
#'                    a population of function closures of 
#'                    genetic operator pipelines which can be 
#'                    executed in parallel. 
#'               \item "PipeG": Pipeline compilation generates  
#'                    a population of genes with embedded 
#'                    genetic operator pipelines which can be 
#'                    executed in parallel. 
#'                    }
#' @param executionModel  Execution model of fitness function evaluation.
#'        Available:
#'        \itemize{
#'        \item "Sequential": \code{base::lapply} is used.
#'        \item "MultiCore":  \code{parallel::mclapply} is used.
#'        \item "MultiCoreHet":  \code{parallel::mclapply} is used. 
#'              For tasks with a high variance in execution time.
#'        \item "FutureApply":  
#'              \code{future.apply::future_lapply} is used.
#'              Requires the specification of a plan.
#'        \item "FutureApplyHet":  
#'              \code{future.apply::future_lapply} is used.
#'              For tasks with a high variance in execution time.
#'              Requires the specification of a plan.
#'        \item "Cluster": \code{parallel::parLapply} is used. 
#'                 Requires a proper configuration of the cluster
#'                 and the specification of an exit handler to 
#'                 shutdown the cluster.
#'        \item "ClusterHet": \code{parallel::parLapplyLB} is used. 
#'                 Requires a proper configuration of the cluster
#'                 and the specification of an exit handler to 
#'                 shutdown the cluster.
#'              For tasks with a high variance in execution time.
#'         }
#'        Default: "Sequential".
#'        
#' @param uParApply       A user-defined parallel apply function
#'                        (e.g. for Rmpi). If specified, overrides 
#'                        settings for \code{executionModel}. 
#'                        Default: \code{NULL}.      
#'
#' @param Cluster         A cluster object generated by 
#'                        \code{parallel::makeCluster()} or
#'                        \code{parallelly::makeCluster()}.
#'                        Default: \code{NULL}.
#' 
#' @param profile   Boolean. 
#'        If \code{TRUE} measures execution time and counts the number of executions
#'        of the main components of genetic algorithms. Default: \code{FALSE}.
#'
#' @param batch    Boolean.
#'        If \code{TRUE}, then save the result in the file
#'        \code{path/xegaResult<exclusive pattern>.rds}. Default: \code{FALSE}.
#'
#' @param anytime  Boolean.
#'        If \code{TRUE}, then save the current best result in the file
#'        \code{path/xegaAnyTimeResult.rds}. Default: \code{FALSE}.
#'
#' @param path
#'        Path. Default: \code{"."}.
#'
#' @param semantics   Determines the representation 
#'                    of the local function list \code{lF}.
#'                    Default: "byValue".
#'                    \itemize{
#'                      \item "byValue": \code{lF} is a named list object.
#'                      \item "byReference": \code{lF} is an environment.
#'                    }
#'
#' @param debug       Boolean. Default: \code{FALSE}. 
#'                    Debug (show) progress in xegaRun main loop.
#'
#' @return Result object. A named list of 
#'         \enumerate{
#'         \item
#'         \code{$popStat}: A matrix with mean, min, Q1, median, Q3, max,
#'                         variance, and median absolute deviation
#'                          of population fitness as columns:
#'                          i-th row for the measures of the i-th generation.
#'         \item 
#'         \code{$fit}: Fitness vector if \code{generations<=1} else: NULL.
#'         \item
#'         \code{$solution}: Named list with fields 
#'         \itemize{
#'         \item
#'         \code{$solution$name}:    Name of problem environment. 
#'         \item
#'         \code{$solution$fitness}: Fitness value of the best solution.
#'         \item
#'         \code{$solution$value}:   The evaluated best gene.
#'         \item
#'         \code{$solution$numberofsolutions}: 
#'                    Number of solutions with the same fitness. 
#'         \item
#'         \code{$solution$genotype}:     The gene is a genetic code. 
#'         \item
#'         \code{$solution$phenotype}:    The decoded gene.
#'         \item
#'         \code{$solution$phenotypeValue}:   The   value of the
#'                          function of the parameters of the solution.
#'         \item 
#'         \code{$solution$evalFail}: Number of failures or fitness evaluations
#'         \item
#'         and, if configured, 
#'         \code{$solution$allgenotypes}, as well as 
#'         \code{$solution$allphenotypes}.
#'         }
#'         \item
#'         \code{$GAconfig}: For rerun with \code{xegaReRun()}.
#'         \item
#'         \code{$GAenv}: Attribute value list of GAconfig.
#'         \item \code{$timer}: An attribute value list with 
#'               the time used (in seconds) in the main blocks of the GA:
#'               tUsed, tInit, tNext, tEval, tObserve, and tSummary.
#'         \item 
#'         \code{$logfn}: File name of logfile. Default: \code{NA}.
#'         \item 
#'         \code{$resfn}: File name of RDS-file with \code{result}. 
#'                       Default: \code{NA}.
#'         \item
#'         \code{$xegaVersion}: xega version used.
#'         }
#'
#' @family Main Program
#'         
#' @examples
#' a<-xegaRun(penv=Parabola2D, generations=3, popsize=5, verbose=0, debug=TRUE)
#' b<-xegaRun(penv=Parabola2D, algorithm="sga", 
#'    generations=10, popsize=20, max=FALSE, 
#'    replication="Kid2Pipeline", crossover="Cross2Gene", pipeline="PipeC",
#'    verbose=1, replay=5, profile=TRUE, debug=FALSE)
#' b1<-xegaRun(penv=Parabola2D, algorithm="sga", 
#'    generations=10, popsize=20, max=FALSE, 
#'    replication="Kid1PipelineG", crossover="Cross2Gene", pipeline="PipeG",
#'    verbose=1, replay=5, profile=TRUE, debug=FALSE)
#' c<-xegaRun(penv=Parabola2D, max=FALSE, algorithm="sgde", 
#'    popsize=20, generations=10, 
#'    mutation="MutateGeneDE", scalefactor="Uniform", crossover="UCrossGene", 
#'    genemap="Identity", replication="DE", 
#'    selection="UniformP", mateselection="UniformP", accept="Best")
#' c1<-xegaRun(penv=Parabola2D, max=FALSE, algorithm="sgde", 
#'    popsize=20, generations=10, elitist=TRUE,
#'    mutation="MutateGeneDE", scalefactor="Uniform", crossover="UCrossGene", 
#'    genemap="Identity", replication="DEPipeline", pipeline="PipeC", 
#'    selection="UniformP", mateselection="UniformP", accept="Best")
#' c2<-xegaRun(penv=Parabola2D, max=FALSE, algorithm="sgde", 
#'    popsize=20, generations=10, elitist=TRUE,
#'    mutation="MutateGeneDE", scalefactor="Uniform", crossover="UCrossGene", 
#'    genemap="Identity", replication="DEPipelineG", pipeline="PipeG", 
#'    selection="UniformP", mateselection="UniformP", accept="Best", debug=FALSE)
#' envXOR<-NewEnvXOR()
#' BG<-compileBNF(booleanGrammar())
#' d<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
#'    generations=4, popsize=20, verbose=0)
#' e<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
#'    generations=4, popsize=20, verbose=0, initgene="InitGeneGe")
#' f<-xegaRun(penv=envXOR, grammar=BG, algorithm="sge", genemap="Mod",  
#'    generations=4, popsize=20, reportEvalErrors=FALSE, verbose=1)
#' g<-xegaRun(penv=envXOR, grammar=BG, max=TRUE, algorithm="sgede", 
#'    popsize=20, generations=4, verbose=1, reportEvalErrors=FALSE,
#'    mutation="MutateGeneDE", scalefactor="Uniform", crossover="UCrossGene", 
#'    genemap="Identity", replication="DE", 
#'    selection="UniformP", mateselection="UniformP", accept="Best")
#' g1<-xegaRun(penv=envXOR, grammar=BG, max=TRUE, algorithm="sgede",
#'    popsize=20, generations=4, verbose=1, reportEvalErrors=FALSE,
#'    mutation="MutateGeneDE", scalefactor="Uniform", crossover="UCrossGene",
#'    genemap="Identity", replication="DEPipelineG", pipeline="PipeG",
#'    selection="UniformP", mateselection="UniformP", accept="Best")
#' h<-xegaRun(penv=lau15, max=FALSE, algorithm="sgperm", 
#'    popsize=20, generations=5, max2opt=20,
#'    genemap="Identity", mutation="MutateGeneMix")
#' i<-xegaRun(penv=lau15, max=FALSE, algorithm="sgperm", 
#'    popsize=20, generations=5, max2opt=20,
#'    genemap="Identity",  mutation="MutateGeneMix", 
#'    executionModel="Sequential", replication="Kid1Pipeline", pipeline="PipeC") 
#' j<-xegaRun(penv=lau15, max=FALSE, algorithm="sgperm", 
#'    popsize=20, generations=5, max2opt=20,
#'    genemap="Identity",  mutation="MutateGeneMix", 
#'    executionModel="Sequential", replication="Kid1PipelineG", pipeline="PipeG", verbose=1) 
#' cat("t(s) h:", h$timer$tMainLoop, "i:", i$timer$tMainLoop, "j:", j$timer$tMainLoop, "\n")  
#' 
#' @importFrom parallelly availableCores
#' @importFrom parallelly supportsMulticore
#' @importFrom xegaSelectGene newCounter
#' @importFrom xegaSelectGene newTimer
#' @importFrom xegaSelectGene Timed
#' @importFrom xegaSelectGene EvalGeneFactory
#' @importFrom xegaSelectGene SelectGeneFactory
#' @importFrom xegaSelectGene ScalingFactory
#' @importFrom xegaSelectGene DispersionMeasureFactory
#' @importFrom xegaSelectGene DispersionRatio
#' @importFrom xegaSelectGene parm
#### TODO
#' @importFrom xegaGeGene xegaGePrecisionFactory
#' @importFrom xegaGeGene mLCMG 
#' @importFrom xegaDfGene xegaDfScaleFactorFactory
#' @importFrom xegaPopulation xegaInitPopulation
#' @importFrom xegaPopulation xegaEvalPopulationFactory
#' @importFrom xegaPopulation xegaAsPipelineFactory
#' @importFrom xegaPopulation xegaObservePopulation
#' @importFrom xegaPopulation xegaSummaryPopulation
#' @importFrom xegaPopulation xegaNextPopulation
#' @importFrom xegaPopulation xegaBestInPopulation
#' @importFrom xegaPopulation xegaConfiguration
#' @importFrom xegaPopulation ApplyFactory
#' @importFrom xegaPopulation CrossRateFactory
#' @importFrom xegaPopulation AcceptFactory
#' @importFrom xegaPopulation CoolingFactory
#' @importFrom xegaPopulation TerminationFactory
#' @importFrom xegaPopulation checkTerminationFactory
#' @importFrom xegaPopulation xegaLogEvalsPopulation
#' @importFrom stats qnorm
#' @importFrom stats var
##### TODO
#' @importFrom xegaPopulation MutationRateFactory 
##### TODO
#' @export
xegaRun<-function(
### Problem Specification
      penv,               # Problem environment. 
      grammar=NULL,       # a grammar object.
      max=TRUE,           # TRUE: max penv$f; FALSE: min penv$f
### Algorithm   
      algorithm="sga",    # "sga", "sgde", "sgperm", "sge", "sgp"
### Basic Parameters
      popsize=100,        # Population size
      generations=20,     # Number of generations
      crossrate=0.2,      # (Probability crossover operator is used)
      mutrate=1.0,        # (Probability mutation operator is used.)
      elitist=TRUE,       # TRUE: Best gene always survives.
      replay=0,           # replay=0: current seed of random number generator.
                          # replay>0: use small integer. 
                          #           Same integer = same seed.	
###
      
### 
	      maxdepth=7,          # maximal depth of a derivation tree
	      maxtrials=5,         # maximal of number of trials of 
                                   # finding subtrees with common root
              codons=25,           # number of codons (GE)
              codonBits=0,         # precision of codon in bits
              codonPrecision="LCM", # Precision methods: 
                                   # "Min", 
                                   # "LCM",
                                   # "MaxPBias".
              maxPBias=0.01, 
		 evalmethod="EvalGeneU",  #
		                     # Evaluation methods:
		                     # "EvalGeneU", 
		                     # "EvalGeneR", 
		                     # "EvalGeneDet", 
		                     # "EvalGeneStoch".
                 evalrep=1,          # Number of repeated evalutions.
                 reportEvalErrors=TRUE, # Report errors in fitness functions.
		                     #
		 genemap="Bin2Dec",  # Gene map for decoding.
		 decoder="DecodeGene",  # Gene decoder. 
		                     #
		 crossrate2=0.3,     # Crossover Rate 2 
		                     # (Probability crossover operator is used)
                 ivcrossrate="Const", # "Const" or "IV"
		 crossover="Cross2Gene", # Crossover operator:
		    #  1 Kid: "CrossGene", UCrossGene, UPCrossGene
		    #  2 Kids: "Cross2Gene", UCross2Gene, UPCross2Gene
		 uCrossSwap=0.2,     # fraction of positions swapped in.
		                     #
                 mincrossdepth=1,      # maximal depth of exchange nodes 
                 maxcrossdepth=7,      # maximal depth of exchange nodes
                                       # for swapping derivation trees
                                     # crossover
                 ivmutrate="Const", # "Const" or "IV"
		 mutrate2=1.0,      # Mutation Rate 2 
		                    # (Probability mutation operator is used.)
		 bitmutrate=0.005,   # bit Mutation Rate
		 bitmutrate2=0.01,   # bit Mutation Rate 2
                 maxmutdepth=3,      # maximal depth of a derivation tree 
                                     # generated by mutation
                 minmutinsertiondepth=1,  # minimal depth of insertion node
                 maxmutinsertiondepth=7,  # maximal depth of insertion node 
		 lambda=0.05,        # decay rate
		 max2opt=100,        # maximal number of trials xegaPermGene.
		 scalefactor1=0.9,   # scale factor (differential evolution)
		 scalefactor2=0.3,   # scale factor (differential evolution)
		 scalefactor="Const", #  scale factor method label 
                                     #
		 cutoffFit=0.5,      # Cutoff percentage for good genes.
		                     #
		 mutation="MutateGene", # Mutation operator:
		                     # "MutateGene" or "IVM"
		                     #
		 replication="Kid2", # Replication method.
                 initgene="InitGene", # Initialization method.
		                     #
		 offset=1,           # offset in proportional selection
		 eps=0.01,           # Small number in proportional selection. 
		 tournamentSize=2,   # size of tournament
		 selectionBias=1.5,  #  selection pressure for Whitleys
		                     #    selective rank selection
		 maxTSR=1.5,         # selection pressure for Grefenstette
		                     #  and Bakers linear rank selection 
		                     # method 
		 selection="SUS", # Selection Method: 
		 mateselection="SUS", # Selection Method: 
		            # "Proportional": proportional to fitness
		            # "PropFitDiff": prop. to fitness diff
		            # "Uniform": with equal probability
		            # "Tournament": tournament selection
		            # "SUS": Baker's stochastic universal selection
		 selectionContinuation=TRUE,
		            #
		 scaling="NoScaling", #  Scaling method: 
		                      #  "NoScaling"
		                      #  "ConstantScaling"   (Static)
		                      #  "ThresholdScaling"  (Dynamic)
		                      #  "ContinuousScaling" (Dynamic)
		 scalingThreshold=0.0, #  Ratio of Dispersion Measures (RDM) 
		                      # in  [1+/-scalingThreshold]: Do not scale!
		 scalingExp=1,        #   For static and threshold scaling (>1) 
		 scalingExp2=1,       #   For threshold scaling  (<1)
		 rdmWeight=1,         #   Weight constant continuous scaling
		 drMax=2.0,           #   Maximum of dispersion ratio
		 drMin=0.5,           #   Minimum of dispersion ratio
		 dispersionMeasure="var",  # Dispersion measure: 
		                     # "var", "std", "mad", "cv", 
		                     # "range", "iqr"
		 scalingDelay=1,     # delay in ratio computation: DM(t)/DM(t-scalingDelay)
		 accept="All",  # accept new gene
		       # Options: "All", "Best", "Metropolis"
		 alpha=0.99,    # Discount rate for temperature.
		 beta=2,        # Constant (in Boltzmann's formula: 
		                # k an arbitrary scaling constant). 
		 cooling="ExponentialMultiplicative",   # accept new gene
		 coolingPower=1, # power of PowerMultiplicativeCooling. 
		                 # Default: Linear! 
		 temp0=40,           # Higher than generations.
		 tempN=0.01,         # Final temperature.
		                     #
		 verbose=1,          # Maximal output per generation displayed.
		                     #
		 logevals=FALSE,     # If TRUE: log evals and parms to file
		 allsolutions=FALSE, # TRUE: All best solutions are returned.
		                     #       at the end of the run.
		 early=FALSE,        # FALSE: Ignore code for early termination.
                 terminationCondition="NoTermination", 
                                      # "NoTermination"  
                                      # "AbsoluteError"  
                                      # "RelativeError"  
                                      # "PAC"  
                                      # "GEQ"  
                                      # "LEQ"  
		 terminationEps=0.01, # fraction of known optimal solution
		                      # for termination interval
                 terminationThreshold=0.0,    #  a threshold for terminating the algorithm  
                 worstFitness=0.0,    # for setting bad fitness for invalid genes    
                PACdelta=0.01,        # For PAC termination condition
                fSpace="Hilbert",     # For PAC Termination condition
                cores=NA,             # Number of cores.
                pipeline="NoPipe",    # Shift evaluation of extended genetic operator pipelines. 
		executionModel="Sequential", 
		                     # Execution models are:
	                             # "Sequential"
	                             # "MultiCore"
		                     # "Cluster"
		                     # needs master, workers, port.
		                     # default: my minimal configuration.
                uParApply=NULL,      # user-defined execution model. 
		Cluster=NULL,        # A cluster object generated by 
                                     # parallel::makeCluster() or
                                     # parallelly::makeCluster() or
		profile=FALSE,       # If TRUE: Measure time spent in
		                     # main blocks of GA.
		batch=FALSE,         # If TRUE: save result to file
		anytime=FALSE,         # If TRUE: save result to file
                path=".",            # path to files.
             semantics="byValue",    # semantics of lF
                debug=FALSE          # debug progress in xegaRun main loop.
		)
{

# Self description for re-running:
# eval(parse(text=GAconfig))
# TODO: More precise reporting on RNG used.

### The following MUST be the first line of the main program.
GAconfiguration<-xegaPopulation::xegaConfiguration("xegaRun", 
					       substitute(penv), 
					       substitute(grammar), 
					       environment())

#parm<-function(x){function() {return(x)}}
# Random number generator: TODO. At the moment just for reporting ... 
# Report RNG 
RGused<-RNGkind("L'Ecuyer-CMRG")
if (replay>0) {set.seed(replay)} else {set.seed(NULL)}
RGseed<-replay

if ((executionModel=="MultiCore") ||
   (executionModel=="MultiCoreHet"))
{ if (parallelly::supportsMulticore()==FALSE) 
     {stop("Execution model MultiCore not supported")} 
if (is.na(cores))  {cores<-parallelly::availableCores()} }

### Tentative code!
if 
    ((executionModel=="Cluster") ||
    (executionModel=="ClusterHet"))
{
if (is.null(Cluster)) 
     {stop("Execution model ", executionModel, 
            " requires a cluster object!")} 
cluster<-xegaSelectGene::parm(Cluster)                     # nocov
}
else
{cluster<-xegaSelectGene::parm(NULL)}

if (max) {MAX<-xegaSelectGene::parm(1)} else {MAX<-xegaSelectGene::parm(-1)}

if (is.null(uParApply))
{parApply<-xegaPopulation::ApplyFactory(method=executionModel)}
else
{parApply<-uParApply}

### shortest representation with potential bias.
if ((algorithm=="sge") && (codonBits==0)) 
{
Precision<-xegaGeGene::xegaGePrecisionFactory(method=codonPrecision)
CodonPrecision<-Precision(grammar$PT$LHS, maxPBias) 
}   
else 
{CodonPrecision<-codonBits}
# We do not check the feasibility of codonPrecision.

bitsOnGene<-codons*CodonPrecision

### CodonPrecision for algorithm sgede:
if (algorithm=="sgede")
{ CodonPrecision<-100*xegaGeGene::mLCMG(grammar$PT$LHS)}

# Build local configuration (local functions) 
lF<-list(
penv=penv,
Grammar=grammar,
MaxDepth=xegaSelectGene::parm(maxdepth),
MaxTrials=xegaSelectGene::parm(maxtrials),
Codons=xegaSelectGene::parm(codons),
CodonPrecision=xegaSelectGene::parm(CodonPrecision),
BitsOnGene=xegaSelectGene::parm(bitsOnGene),
MutationRate=xegaPopulation::MutationRateFactory(method=ivmutrate),
MutationRate1=xegaSelectGene::parm(mutrate),
MutationRate2=xegaSelectGene::parm(mutrate2),
BitMutationRate1=xegaSelectGene::parm(bitmutrate),
BitMutationRate2=xegaSelectGene::parm(bitmutrate2),
MaxMutDepth=xegaSelectGene::parm(maxmutdepth),
MinMutInsertionDepth=xegaSelectGene::parm(minmutinsertiondepth),
MaxMutInsertionDepth=xegaSelectGene::parm(maxmutinsertiondepth),
Lambda=xegaSelectGene::parm(lambda),
Max2Opt=xegaSelectGene::parm(max2opt),
ScaleFactor1=xegaSelectGene::parm(scalefactor1),
ScaleFactor2=xegaSelectGene::parm(scalefactor2),
ScaleFactor=xegaDfGene::xegaDfScaleFactorFactory(method=scalefactor),
CutoffFit=xegaSelectGene::parm(cutoffFit),
CBestFitness=xegaSelectGene::parm(0.0),
CMeanFitness=xegaSelectGene::parm(0.0),
CVarFitness=xegaSelectGene::parm(0.0),
CWorstFitness=xegaSelectGene::parm(worstFitness),
CrossRate=xegaPopulation::CrossRateFactory(method=ivcrossrate),
CrossRate1=xegaSelectGene::parm(crossrate),
CrossRate2=xegaSelectGene::parm(crossrate2),
UCrossSwap=xegaSelectGene::parm(uCrossSwap),
MinCrossDepth=xegaSelectGene::parm(mincrossdepth),
MaxCrossDepth=xegaSelectGene::parm(maxcrossdepth),
Max=MAX,
Offset=xegaSelectGene::parm(offset),
Eps=xegaSelectGene::parm(eps),
TerminationEps=xegaSelectGene::parm(terminationEps), 
TerminationThreshold=xegaSelectGene::parm(terminationThreshold), 
Accept=xegaPopulation::AcceptFactory(method=accept),
Alpha=xegaSelectGene::parm(alpha),
Beta=xegaSelectGene::parm(beta),
Cooling=xegaPopulation::CoolingFactory(method=cooling),
CoolingPower=xegaSelectGene::parm(coolingPower),
Generations=xegaSelectGene::parm(generations),
cGeneration=xegaSelectGene::parm(0),
Temp0=xegaSelectGene::parm(temp0),
TempK=xegaSelectGene::parm(temp0),
TempN=xegaSelectGene::parm(tempN),
Elitist=xegaSelectGene::parm(elitist),
Selection = xegaSelectGene::parm(selection),
MateSelection = xegaSelectGene::parm(mateselection),
SelectionContinuation = xegaSelectGene::parm(selectionContinuation),
ScalingFitness = xegaSelectGene::ScalingFactory(method=scaling),
ScalingExp = xegaSelectGene::parm(scalingExp),
ScalingExp2 = xegaSelectGene::parm(scalingExp2),
DispersionMeasure = xegaSelectGene::DispersionMeasureFactory(method=dispersionMeasure),
RDM = xegaSelectGene::parm(1.0),
DRmax = xegaSelectGene::parm(drMax),
DRmin = xegaSelectGene::parm(drMin),
RDMWeight = parm(rdmWeight),
ScalingThreshold = xegaSelectGene::parm(scalingThreshold),
ScalingDelay = xegaSelectGene::parm(scalingDelay),
AllSolutions=xegaSelectGene::parm(allsolutions),
Verbose=xegaSelectGene::parm(verbose),
TournamentSize=xegaSelectGene::parm(tournamentSize),
SelectionBias=xegaSelectGene::parm(selectionBias),
MaxTSR=xegaSelectGene::parm(maxTSR),
SelectGene=xegaSelectGene::SelectGeneFactory(method=selection),
SelectMate=xegaSelectGene::SelectGeneFactory(method=mateselection),
MutateGene=sgXMutationFactory(algorithm=algorithm, method=mutation), # gene dependent
CrossGene=sgXCrossoverFactory(algorithm=algorithm, method=crossover), # gene dependent
InitGene=sgXInitGeneFactory(algorithm, method=initgene),  # gene dependent
DecodeGene=sgXDecodeGeneFactory(algorithm, method=decoder), # gene dependent 
GeneMap=sgXGeneMapFactory(algorithm=algorithm, method=genemap), # gene dependent
EvalGene=xegaSelectGene::EvalGeneFactory(method=evalmethod),
rep=xegaSelectGene::parm(evalrep),
ReportEvalErrors=xegaSelectGene::parm(reportEvalErrors),
ReplicateGene=sgXReplicationFactory(algorithm=algorithm, method=replication), # gene dependent
PACdelta=xegaSelectGene::parm(PACdelta), 
Cores=xegaSelectGene::parm(cores), # number of cores
Pipeline=xegaSelectGene::parm(pipeline), 
AsPipeline=xegaPopulation::xegaAsPipelineFactory(pipeline),
lapply=parApply,
cluster=cluster,
path=xegaSelectGene::parm(path)
)

# Configure timing.
# Get timers.
mainLoopTimer<-xegaSelectGene::newTimer()
initPopulationTimer<-xegaSelectGene::newTimer()
evalPopulationTimer<-xegaSelectGene::newTimer()
observePopulationTimer<-xegaSelectGene::newTimer()
summaryPopulationTimer<-xegaSelectGene::newTimer()
nextPopulationTimer<-xegaSelectGene::newTimer()

if (evalrep==1)
{evalpopfn<-xegaPopulation::xegaEvalPopulationFactory(method="EvalPopulation")}
else
{evalpopfn<-xegaPopulation::xegaEvalPopulationFactory(method="RepEvalPopulation")}

if (profile==TRUE)
{
InitPopulation<-xegaSelectGene::Timed(xegaPopulation::xegaInitPopulation, initPopulationTimer)
EvalPopulation<-xegaSelectGene::Timed(evalpopfn, evalPopulationTimer)
ObservePopulation<-xegaSelectGene::Timed(xegaPopulation::xegaObservePopulation, observePopulationTimer)
SummaryPopulation<-xegaSelectGene::Timed(xegaPopulation::xegaSummaryPopulation, summaryPopulationTimer)
NextPopulation<-xegaSelectGene::Timed(xegaPopulation::xegaNextPopulation, nextPopulationTimer)
}

if (profile==FALSE)
{
InitPopulation<-xegaPopulation::xegaInitPopulation
EvalPopulation<-evalpopfn
ObservePopulation<-xegaPopulation::xegaObservePopulation
SummaryPopulation<-xegaPopulation::xegaSummaryPopulation
NextPopulation<-xegaPopulation::xegaNextPopulation
}

# RunGA Main 

### Semantics
if (semantics=="byReference") {lF<-as.environment(lF)}

###

tUsed<-mainLoopTimer()

xegaDebug(debug, msg="xegaDebug: InitPopulation")

pop<-InitPopulation(popsize, lF)

xegaDebug(debug, msg="xegaDebug: lF$AsPipeline")

pop<-lF$AsPipeline(pop, lF)

xegaDebug(debug, msg="xegaDebug: pop[[1]]", obj=pop[[1]])

xegaDebug(debug, msg="xegaDebug: EvalPopulation")

popfit<-EvalPopulation(pop, lF)

pop<-popfit$pop
fit<-popfit$fit
evalFail<-popfit$evalFail

xegaDebug(debug, msg="xegaDebug: ObservePopulation")

popStat<-ObservePopulation(fit)

if (logevals==TRUE)
{evallog<-xegaLogEvalsPopulation(pop=pop, evallog=list(), generation=0, lF=lF)} # nocov  

# start Termination conditions.
### Tentative.
# TODO: The interface is too restricted. 
#       Needs to see e.g. history of solutions, known optima, 
#                         distance from reference points.
if (early && ("terminate" %in% names(penv)))
{ Terminate<-penv$terminate 
}  # nocov
else
{ Terminate<-xegaPopulation::TerminationFactory(terminationCondition); 
  checkTerminate<-xegaPopulation::checkTerminationFactory(terminationCondition)
  checklst<-checkTerminate(penv, max)
  lF$penv<-checklst$penv
}

if (terminationCondition=="PAC") 
{ ### a patch.
if (fSpace=="Hilbert") 
{
PACopt<-popStat[1]+sqrt(popStat[7])*stats::qnorm(PACdelta, lower.tail=FALSE)
lF$PACopt<-xegaSelectGene::parm(PACopt)
}
}

#### end Termination conditions.

# skip main loop, if generations == 1 (random sample)!
if (generations>1)
{
for(i in 1:generations)
{
if (anytime==TRUE) 
{ tUsed<-mainLoopTimer(); tUsed<-mainLoopTimer(); 
      xegaAnyTimeResult(mainLoopTimer, pp=pop, ft=fit, lF=lF, 
               allsolutions=allsolutions, popStat=popStat, evalFail=evalFail, 
               GAconfiguration=GAconfiguration, path=path)} 

xegaDebug(debug, msg=paste0("xegaDebug: Main Loop: Generation ", i))

xegaDebug(debug, msg="xegaDebug: Main Loop [SummaryPopulation]")

	rc<-SummaryPopulation(pop, fit, lF, i)

# for adaptive operators.
        lF$CBestFitness<-xegaSelectGene::parm(max(fit))
        lF$CMeanFitness<-xegaSelectGene::parm(mean(fit))
        lF$CVarFitness<-xegaSelectGene::parm(stats::var(fit))
        lF$CWorstFitness<-xegaSelectGene::parm(min(fit))

	if (scaling %in% c("ThresholdScaling", "ContinuousScaling"))
	{lF$RDM<-xegaSelectGene::parm(xegaSelectGene::DispersionRatio(
		matrix(popStat, byrow=TRUE, ncol=8), lF$DispersionMeasure, lF))
	       }

xegaDebug(debug, msg="xegaDebug: Main Loop [NextPopulation]")

	pop<-NextPopulation(pop, lF$ScalingFitness(fit, lF), lF)
        lF$cGeneration<-xegaSelectGene::parm(i)

xegaDebug(debug, msg="xegaDebug: pop[[1]]", obj=pop[[1]])
xegaDebug(debug, msg="xegaDebug: pop[[2]]", obj=pop[[2]])

xegaDebug(debug, msg="xegaDebug: Main Loop [EvalPopulation]")

	popfit<-EvalPopulation(pop, lF)

	pop<-popfit$pop
	fit<-popfit$fit

	evalFail<-evalFail+popfit$evalFail
	if (length(fit)<popsize) 
	{warning("fit and pop do not match. Gene representation OK?") 
         return(popfit)} # nocov

xegaDebug(debug, msg="xegaDebug: Main Loop [ObservePopulation]")

	popStat<-ObservePopulation(fit, popStat)
if (logevals==TRUE)
{evallog<-xegaLogEvalsPopulation(pop=pop, evallog=evallog, generation=i, lF=lF)} # nocov  

        if (Terminate(xegaBestInPopulation(pop, fit, lF, FALSE), lF)==TRUE) 
		{generations<-i;break}    # nocov

	# Cooling schedule for Metropolis acceptance rule. Abstract out?
#	cat("Temperature:", lF$TempK(), "\n")
#	cat("new Temperature:", lF$TempK(), "\n")
	newTemperature<-force(lF$Cooling(i, lF))
#	cat("new Temperature:", newTemperature, "\n")
	lF$TempK<-parm(newTemperature)
###  Before end of main loop.

# end of main loop
}
# end of skip
}

# set up return values.

	rc<-SummaryPopulation(pop, fit, lF, generations)

tUsed<-mainLoopTimer()

	timer=list()
	timer[["tMainLoop"]]<-mainLoopTimer("TimeUsed")
	timer[["tInitPopulation"]]<-initPopulationTimer("TimeUsed")
	timer[["tNextPopulation"]]<-nextPopulationTimer("TimeUsed")
	timer[["tEvalPopulation"]]<-evalPopulationTimer("TimeUsed")
	timer[["tObservePopulation"]]<-observePopulationTimer("TimeUsed")
	timer[["tSummaryPopulation"]]<-summaryPopulationTimer("TimeUsed")
	timer[["cMainLoop"]]<-mainLoopTimer("Count")
	timer[["cInitPopulation"]]<-initPopulationTimer("Count")
	timer[["cNextPopulation"]]<-nextPopulationTimer("Count")
	timer[["cEvalPopulation"]]<-evalPopulationTimer("Count")
	timer[["cObservePopulation"]]<-observePopulationTimer("Count")
	timer[["cSummaryPopulation"]]<-summaryPopulationTimer("Count")

        rc<-xegaPopulation::xegaBestInPopulation(pop, fit, lF, allsolutions)

       if (generations>1) {fit=NULL}

   popS<-matrix(popStat, byrow=TRUE, ncol=8)
   colnames(popS)<-c("mean", "min", "Q1", "median", "Q3", "max", "var", "mad")

   GAconfiguration$GAenv$cores<-cores

   result<-list(popStat=popS,
		    fit=fit,
                    solution=rc,
		    evalFail=evalFail,
                    GAconfig=list(GAconfiguration$GAconf),
                    GAenv=GAconfiguration$GAenv,
                    timer=timer, 
                    logfn=NA,
                    resfn=NA, 
                    xegaVersion=xegaVersion(verbose=FALSE))

if (lF$Verbose()==1)  {cat("\n")}

if (logevals==TRUE)
{
fn<-createExclusiveFile(fpath=path, prefix="xegaEvalLog", ext=".rds") # nocov
    if (is.null(fn))                                    # nocov
        stop("Cannot create an exclusive file.")          # nocov
      saveRDS(object=evallog, file=fn)                    # nocov
      result$logfn<-fn                                    # nocov
}

if (batch==TRUE)
{
  fn<-createExclusiveFile(fpath=path, prefix="xegaResult", ext=".rds") # nocov
    if (is.null(fn))                                      # nocov
        stop("Cannot create an exclusive file.")            # nocov
        result$resfn<-fn                                    # nocov
        saveRDS(object=result, file=fn)                    # nocov
}

        return(result)

}

