
#' Factory for configuring a gene-dependent InitGene function.
#'
#' @param algorithm Algorithm. 
#'                  Available: "sga", "sgde", "sgperm", "sge", sgp". 
#'                  Default: "sga".
#'
#' @param method    Method. Default: "InitGene".
#'                  For sgp, method = "InitGene" or "InitGeneGe".
#'
#' @return InitGene function from the correct package.
#'
#' @family Configuration
#'
#' @examples
#' sgXInitGeneFactory(algorithm="sgperm")
#'
#'@importFrom xegaGaGene xegaGaInitGene
#'@importFrom xegaGpGene xegaGpInitGeneFactory
#'@importFrom xegaGeGene xegaGeInitGene
#'@importFrom xegaDfGene xegaDfInitGene
#'@importFrom xegaPermGene xegaPermInitGene
#'@export
sgXInitGeneFactory<-function(algorithm="sga", method="InitGene")
{
   if (algorithm=="sga") {f<-xegaGaGene::xegaGaInitGene}
   if (algorithm=="sgp") {f<-xegaGpGene::xegaGpInitGeneFactory(method)}
   if (algorithm=="sge") {f<-xegaGeGene::xegaGeInitGene}
   if (algorithm=="sgde") {f<-xegaDfGene::xegaDfInitGene}
   if (algorithm=="sgperm") {f<-xegaPermGene::xegaPermInitGene}
if (!exists("f", inherits=FALSE))
        {stop("sgX InitGene Factory label ", algorithm, " does not exist")}
return(f)
}
	
