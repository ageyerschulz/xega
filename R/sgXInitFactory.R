
#' Factory for configuring a gene-dependent InitGene function.
#'
#' @param algorithm Algorithm. 
#'                  Available: "sga", "sgde", "sgperm", "sge", sgp". 
#'                  Default: "sga".
#'
#' @param method    Method. Default: "InitGene".
#'
#' @return InitGene function from the correct package.
#'
#' @family Configuration
#'
#' @examples
#' sgXInitGeneFactory(algorithm="sgperm")
#'
#'@importFrom xegaGaGene xegaGaInitGene
#'@importFrom xegaGpGene xegaGpInitGene
#'@importFrom xegaGeGene xegaGeInitGene
#'@importFrom xegaDfGene xegaDfInitGene
#'@importFrom xegaPermGene xegaPermInitGene
#'@export
sgXInitGeneFactory<-function(algorithm="sga", method="InitGene")
{
   if (algorithm=="sga") {f<-xegaGaGene::xegaGaInitGene}
   if (algorithm=="sgp") {f<-xegaGpGene::xegaGpInitGene}
   if (algorithm=="sge") {f<-xegaGeGene::xegaGeInitGene}
   if (algorithm=="sgde") {f<-xegaDfGene::xegaDfInitGene}
   if (algorithm=="sgperm") {f<-xegaPermGene::xegaPermInitGene}
if (!exists("f", inherits=FALSE))
        {stop("sgX InitGene Factory label ", algorithm, " does not exist")}
return(f)
}
	
