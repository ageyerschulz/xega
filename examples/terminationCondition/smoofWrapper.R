
#
# We import xegaRunf from xega.
#

#' @inherit xega::Parabola2D title description
#' @importFrom xega Parabola2D
#' @export Parabola2D
Parabola2D<-xega::Parabola2D

#' @inherit xega::NewEnvXOR title description
#' @importFrom xegaDerivationTrees treeLeaves
#' @importFrom xega NewEnvXOR
#' @export NewEnvXOR
NewEnvXOR<-xega::NewEnvXOR

#' @inherit xega::compileBNF
#' @importFrom xega compileBNF
#' @export compileBNF
compileBNF<-xega::compileBNF

#' @inherit xega::booleanGrammar title description
#' @importFrom xega booleanGrammar
#' @export booleanGrammar
booleanGrammar<-xega::booleanGrammar

#' @inherit xega::lau15 title description
#' @importFrom xega lau15
#' @export lau15
lau15<-xega::lau15

#' @inherit xega::xegaRun
#'
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
#' @importFrom xegaGeGene xegaGePrecisionFactory
#' @importFrom xegaDfGene xegaDfScaleFactorFactory
#' @importFrom xegaPopulation xegaInitPopulation
#' @importFrom xegaPopulation xegaEvalPopulation
#' @importFrom xegaPopulation xegaObservePopulation
#' @importFrom xegaPopulation xegaSummaryPopulation
#' @importFrom xegaPopulation xegaNextPopulation
#' @importFrom xegaPopulation xegaBestInPopulation
#' @importFrom xegaPopulation xegaConfiguration
#' @importFrom xegaPopulation ApplyFactory
#' @importFrom xegaPopulation CrossRateFactory
#' @importFrom xegaPopulation AcceptFactory
#' @importFrom xegaPopulation CoolingFactory
#' @importFrom xegaPopulation xegaLogEvalsPopulation
##### TODO
#' @importFrom xegaPopulation MutationRateFactory 
#'@export              
xegaRun<-xega::xegaRun

#
# We import 5 function factories from package smoof.
#

#' Function factory for the Rastrigin Function
#'
#' @description See \code{makeRastriginFunction()} in package \code{smoof}. 
#'              <https://CRAN.R-project.org/package=smoof>.
#'
#' @references Bossek, Jakob (2017)   
#'             smoof: Single- and Multi-Objective Optimization Test Functions,
#'             R-Journal, 9(1), pp. 103-113. 
#'             <doi:10.32614/RJ-2017-004>
#'
#' @param dimensions Integer. Number of dimension of parameter space.
#'
#' @return The Rastrigin function with \code{dimensions} dimensions.
#'
#' @examples
#' F1<-makeRastriginFunction(2)
#' F1(c(0.0, 0.0))
#' @importFrom smoof makeRastriginFunction
#' @export
makeRastriginFunction<-smoof::makeRastriginFunction

#' Function factory for the Rosenbrock Function
#'
#' @description See \code{makeRosenbrockFunction()} in package \code{smoof}. 
#'              <https://CRAN.R-project.org/package=smoof>.
#'
#' @references Bossek, Jakob (2017)   
#'             smoof: Single- and Multi-Objective Optimization Test Functions,
#'             R-Journal, 9(1), pp. 103-113. 
#'             <doi:10.32614/RJ-2017-004>
#'
#' @param dimensions Integer. Number of dimension of parameter space.
#'
#' @return The Rosenbrock function with \code{dimensions} dimensions.
#'
#' @examples
#' F2<-makeRosenbrockFunction(3)
#' F2(c(0.0, 0.0, 0.0))
#' @importFrom smoof makeRosenbrockFunction
#' @export
makeRosenbrockFunction<-smoof::makeRosenbrockFunction

#' Function factory for the sphere Function
#'
#' @description See \code{makeSphereFunction()} in package \code{smoof}. 
#'              <https://CRAN.R-project.org/package=smoof>.
#'
#' @references Bossek, Jakob (2017)   
#'             smoof: Single- and Multi-Objective Optimization Test Functions,
#'             R-Journal, 9(1), pp. 103-113. 
#'             <doi:10.32614/RJ-2017-004>
#'
#' @param dimensions Integer. Number of dimension of parameter space.
#'
#' @return The sphere function with \code{dimensions} dimensions.
#'
#' @examples
#' F3<-makeSphereFunction(4)
#' F3(c(0.0, 0.0, 0.0, 0.0))
#' @importFrom smoof makeSphereFunction
#' @export
makeSphereFunction<-smoof::makeSphereFunction

#' Function factory for the Schwefel Function
#'
#' @description See \code{makeSchwefelFunction()} in package \code{smoof}. 
#'              <https://CRAN.R-project.org/package=smoof>.
#'
#' @references Bossek, Jakob (2017)   
#'             smoof: Single- and Multi-Objective Optimization Test Functions,
#'             R-Journal, 9(1), pp. 103-113. 
#'             <doi:10.32614/RJ-2017-004>
#'
#' @param dimensions Integer. Number of dimension of parameter space.
#'
#' @return The Schwefel function with \code{dimensions} dimensions.
#'
#' @examples
#' F4<-makeSchwefelFunction(4)
#' F4(c(0.0, 0.0, 0.0, 0.0))
#' @importFrom smoof makeSchwefelFunction
#' @export
makeSchwefelFunction<-smoof::makeSchwefelFunction

#' Function factory for the Shekel Function
#'
#' @description See \code{makeShekelFunction()} in package \code{smoof}. 
#'              <https://CRAN.R-project.org/package=smoof>.
#'
#' @references Bossek, Jakob (2017)   
#'             smoof: Single- and Multi-Objective Optimization Test Functions,
#'             R-Journal, 9(1), pp. 103-113. 
#'             <doi:10.32614/RJ-2017-004>
#'
#' @param m      Integer. Number of dimension of parameter space.
#'               \code{m} must be \code{5}, \code{7}, or \code{10}. 
#'
#' @return The Shekel function with \code{dimensions} dimensions.
#'
#' @examples
#' F5<-makeShekelFunction(m=5)
#' F5(c(0.0, 0.0, 0.0, 0.0))
#' @importFrom smoof makeShekelFunction
#' @export
makeShekelFunction<-smoof::makeShekelFunction

#' Factory for wrapping smoof functions.
#' 
#' @description A minimal wrapper for smoof functions. 
#'          This is an R-example for the facade architectural pattern.
#'
#' @details The R-package smoof provides a large number 
#'          of benchmark and test functions for single and 
#'          multiobjective optimization. See
#'         <https://cran.r-project.org/web/packages/smoof/index.html>
#'
#' @param smoofFN       Single-objective smoof function 
#'                      (usually a call to the generator of a 
#'                       smoof function).
#'
#' @references Bossek, Jakob (2017)
#'          smoof: Single- and Multi-Objective Optimization Test Functions.
#'           The R Journal, 9(1), 103-113. 
#'
#' @return A problem environment (a list of functions):
#'         \itemize{
#'         \item \code{$name()}: Function name.
#'         \item \code{$bitlength()}: Vector. Number of bits of each parameter.
#'                                    
#'         \item \code{$genelength()}:  Integer. Number of bits of gene.
#'         \item \code{$lb()}:     Vector. Lower bounds of parameters.
#'         \item \code{$ub()}:     Vector. Upper bounds of parameters.
#'         \item \code{$f(parm, gene=0, lF=0)}: The fitness function.
#'         \item \code{$max()}:    Boolean. 
#'                If \code{TRUE}, a maximization problem.
#'         \item \code{$globalOptimum()}: Named list with fields
#'            \itemize{
#'            \item \code{$param}: Parameter vector of global optimum.
#'            \item \code{$value}: Function value of global optimum.
#'            \item \code{$is.minimum}: Boolean.        
#'            }
#'         }
#'         For details, see the interface description
#'         in the package description.
#' 
#' @family problem environments
#'
#' @examples
#' t1<-smoofWrapperFactory(makeSchwefelFunction(10))
#' t1$f(rep(0.0, 10))
#' @importFrom smoof getName
#' @importFrom smoof getNumberOfParameters
#' @importFrom smoof getLowerBoxConstraints
#' @importFrom smoof getUpperBoxConstraints
#' @importFrom smoof shouldBeMinimized
#' @importFrom smoof getGlobalOptimum
#' @export
smoofWrapperFactory<-function(smoofFN)
{
   parm<-function(x){function() {return(x)}}
   self<-list()
   self$name<-parm(smoof::getName(smoofFN))
   self$bitlength<-parm(rep(64, smoof::getNumberOfParameters(smoofFN)))
   self$genelength=function() {sum(self$bitlength())}
   self$lb<-parm(as.vector(smoof::getLowerBoxConstraints(smoofFN)))
   self$ub<-parm(as.vector(smoof::getUpperBoxConstraints(smoofFN)))
   self$f<-function(param, gene=0, lF=0) {smoofFN(param) }
   self$max<-parm(!(smoof::shouldBeMinimized(smoofFN)))
   self$globalOptimum<-parm((smoof::getGlobalOptimum(smoofFN)))
   # evaluate promises.
   t<-self$name()
   t<-self$bitlength()
   t<-self$genelength()
   t<-self$ub()
   t<-self$f(self$lb())
   t<-self$max()
   t<-self$globalOptimum()
   return(self)
}

