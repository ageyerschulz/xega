
#' Factory for configuring a gene-dependent geneMap function.
#'
#' @description The geneMap function depends on the gene representation and
#'              the algorithm selected.
#'
#' @details Methods available for the different algorithms are:
#'    \itemize{
#'     \item "sga": "Bin2Dec", "Gray2Dec", "Identity", "Permutation".
#'     \item "sgde": "Identity".
#'     \item "sgperm": "Identity". The gene map function is not used in 
#'                                 the decoder.
#'     \item "sgp": "Identity". The gene map function is not used in 
#'                                 the decoder.
#'     \item "sge": "Mod" or "Bucket".
#'     \item "sgede": "Identity".
#'  }
#'
#' @param algorithm  Algorithm. 
#'                   Available: "sga", "sgde", "sgperm", "sge", sgp". 
#'                   Default: "sga".
#'
#' @param method     The GeneMap method. The choices depend on the 
#'                   \code{algorithm}. 
#'
#' @return GeneMap function for the selected algorithm from the correct package.
#'
#' @family Configuration
#'
#' @examples
#' sgXGeneMapFactory(algorithm="sga", method="Bin2Dec")
#'
#'@importFrom xegaGaGene xegaGaGeneMapFactory
#'@importFrom xegaGeGene xegaGeGeneMapFactory
#'@importFrom xegaDfGene xegaDfGeneMapFactory
#'@export
sgXGeneMapFactory<-function(algorithm="sga", method="Bin2Dec")
{
   if (algorithm=="sga") 
       {Factory<-xegaGaGene::xegaGaGeneMapFactory}
   if (algorithm=="sgp") 
       {Factory<-xegaGaGene::xegaGaGeneMapFactory}
   if (algorithm=="sge") 
       {Factory<-xegaGeGene::xegaGeGeneMapFactory}
   if (algorithm=="sgde") {Factory<-xegaDfGene::xegaDfGeneMapFactory}
   if (algorithm=="sgede") {Factory<-xegaDfGene::xegaDfGeneMapFactory}
   if (algorithm=="sgperm") 
       {Factory<-xegaDfGene::xegaDfGeneMapFactory}
if (!exists("Factory", inherits=FALSE))
        {stop("sgX GeneMap Factory label ", algorithm, " does not exist")}
return(Factory(method))
}
	
