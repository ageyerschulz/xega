
#' Factory for configuring a gene-dependent Replication function.
#'
#' @param algorithm  Algorithm. 
#'                   Available: "sga", "sgde", "sgperm", "sge", "sgede", sgp". 
#'                   Default: "sga".
#'
#' @param method     Method. 
#'                   
#'                   Options are package-dependent:
#'    \itemize{
#'     \item "sga", "sgperm", "sge", sgp": 
#'        "Kid1", "KidPipeline", "Kid2".
#'     \item "sgde", "sgede": 
#'        "DE", "DEPipeline".
#'  }
#'
#' @family Configuration
#'
#' @examples
#' sgXReplicationFactory(algorithm="sgp", method="Kid1")
#'
#' @return A replication function for the algorithm from the correct package.
#'
#'@importFrom xegaGaGene xegaGaReplicationFactory
#'@importFrom xegaDfGene xegaDfReplicationFactory
#'@export
sgXReplicationFactory<-function(algorithm="sga", method="Kid1")
{
   if (algorithm=="sga") 
       {Factory<-xegaGaGene::xegaGaReplicationFactory}
   if (algorithm=="sgp") 
       {Factory<-xegaGaGene::xegaGaReplicationFactory}
   if (algorithm=="sge") 
       {Factory<-xegaGaGene::xegaGaReplicationFactory}
   if (algorithm=="sgde") {Factory<-xegaDfGene::xegaDfReplicationFactory}
   if (algorithm=="sgede") {Factory<-xegaDfGene::xegaDfReplicationFactory}
   if (algorithm=="sgperm") 
       {Factory<-xegaGaGene::xegaGaReplicationFactory}
if (!exists("Factory", inherits=FALSE))
        {stop("sgX Replication Factory label ", algorithm, " does not exist")}
return(Factory(method))
}
	
