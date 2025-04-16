
#' Factory for configuring a gene-dependent Crossover function.
#'
#' @description \code{sgXCrossoverFactory()} selects 
#'              \enumerate{
#'              \item the algorithm-specific crossover factory and 
#'              \item the method in this factory. 
#'              }
#'
#' @details The available methods for each algorithm are: 
#'    \itemize{
#'     \item "sga": 
#'        "Cross2Gene", "UCross2Gene", "UPCross2Gene",
#'        "CrossGene", "UCrossGene", "UPCrossGene".
#'     \item "sge": 
#'        "Cross2Gene", "UCross2Gene", "UPCross2Gene",
#'        "CrossGene", "UCrossGene", "UPCrossGene".
#'     \item "sgede": 
#'        "CrossGene", "UCrossGene", "UPCrossGene".
#'     \item "sgp": 
#'        "CrossGene", "Cross2Gene", "AllCrossGene", "AllCross2Gene",
#'        "FilterCrossGene", "FilterCross2Gene".
#'     \item "sgde": 
#'        "CrossGene", "UCrossGene", "UPCrossGene".
#'     \item "sgperm": 
#'        "CrossGene", "Cross2Gene".
#'  }
#'
#' @param algorithm  Specifies algorithm. 
#'                   Available: "sga", "sgde", "sgperm", "sge", sgp". 
#'                   Default: "sga".
#' @param method     Crossover method.  Algorithm (gene representation) 
#'                   dependent. Default: \code{CrossGene()}. 
#'                   Must be available in the gene-specific 
#'                   crossover factories.
#' @return  Crossover function from the crossover factory of 
#'                    the selected package.
#'
#' @family Configuration
#'
#' @examples
#' sgXCrossoverFactory(algorithm="sga", method="CrossGene")
#'
#'@importFrom xegaGaGene xegaGaCrossoverFactory
#'@importFrom xegaGpGene xegaGpCrossoverFactory
#'@importFrom xegaDfGene xegaDfCrossoverFactory
#'@importFrom xegaPermGene xegaPermCrossoverFactory
#'@export
sgXCrossoverFactory<-function(algorithm="sga", method="CrossGene")
{
   if (algorithm=="sga") {Factory<-xegaGaGene::xegaGaCrossoverFactory}
   if (algorithm=="sgp") {Factory<-xegaGpGene::xegaGpCrossoverFactory}
   if (algorithm=="sge") {Factory<-xegaGaGene::xegaGaCrossoverFactory}
   if (algorithm=="sgede") {Factory<-xegaDfGene::xegaDfCrossoverFactory}
   if (algorithm=="sgde") {Factory<-xegaDfGene::xegaDfCrossoverFactory}
   if (algorithm=="sgperm") {Factory<-xegaPermGene::xegaPermCrossoverFactory}
if (!exists("Factory", inherits=FALSE))
        {stop("sgX Crossover Factory label ", algorithm, " does not exist")}
return(Factory(method))
}
	
