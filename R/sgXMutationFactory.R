
#' Factory for configuring a gene-dependent Mutation function.
#'
#' @description \code{sgXMutationFactory()} selects 
#'              \enumerate{
#'              \item the algorithm-specific mutation factory and 
#'              \item the method in this factory. 
#'              }
#'
#' @details The available methods for each factory are:
#'    \itemize{
#'     \item "sga": "MutateGene", "IVM".
#'     \item "sge": "MutateGene", "IVM".
#'     \item "sgp": "MutateGene", "MutateAllGene", "MutateFilterGene".
#'     \item "sgede": "MutateGene", "MutateGeneDE".
#'     \item "sgde": "MutateGene", "MutateGeneDE".
#'     \item "sgperm": "MutateGene", "MutateGeneOrderBased", 
#'           "MutateGenekInversion", "MutateGene2Opt", "MutateGenekOptLK",
#'           "MutateGeneGreedy", "MutateGeneBestGreedy", "MutateGeneMix".
#'  }
#'
#' @param algorithm  Algorithm. 
#'                   Available: "sga", "sgde", "sgperm", "sge", sgp". 
#'                   Default: "sga".
#'
#' @param method     Method. Available methods are package-dependent.
#'
#' @return MutateGene  function for the selected  algorithm 
#'                     from the correct package.
#'
#' @family Configuration
#'
#'@examples
#' sgXMutationFactory(algorithm="sga", method="MutateGene")
#' 
#'@importFrom xegaGaGene xegaGaMutationFactory
#'@importFrom xegaGpGene xegaGpMutationFactory
#'@importFrom xegaDfGene xegaDfMutationFactory
#'@importFrom xegaPermGene xegaPermMutationFactory
#'@export
sgXMutationFactory<-function(algorithm="sga", method="MutateGene")
{
   if (algorithm=="sga") 
       {Factory<-xegaGaGene::xegaGaMutationFactory}
   if (algorithm=="sgp") 
       {Factory<-xegaGpGene::xegaGpMutationFactory}
   if (algorithm=="sge") 
       {Factory<-xegaGaGene::xegaGaMutationFactory}
   if (algorithm=="sgede") {Factory<-xegaDfGene::xegaDfMutationFactory}
   if (algorithm=="sgde") {Factory<-xegaDfGene::xegaDfMutationFactory}
   if (algorithm=="sgperm") 
       {Factory<-xegaPermGene::xegaPermMutationFactory}
if (!exists("Factory", inherits=FALSE))
        {stop("sgX Mutation Factory label ", algorithm, " does not exist")}
return(Factory(method))
}
	
