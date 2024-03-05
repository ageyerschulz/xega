
#' Factory for configuring a gene dependent Replication function.
#'
#' @param algorithm  Algorithm. 
#'                   Available: "sga", "sgde", "sgperm", "sge", sgp". 
#'                   Default: "sga".
#'
#' @param method     Method. 
#'                   
#'                   Options are package-dependent:
#'    \itemize{
#'     \item "sga", "sgperm", "sge", sgp": 
#'        "Kid1", "Kid2".
#'     \item "sgde": 
#'        "DE".
#'  }
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
   if (algorithm=="sgperm") 
       {Factory<-xegaGaGene::xegaGaReplicationFactory}
if (!exists("Factory", inherits=FALSE))
        {stop("sgX Replication Factory label ", algorithm, " does not exist")}
return(Factory(method))
}
	
