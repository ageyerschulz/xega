
#' Factory for configuring a gene-dependent DecodeGene function.
#'
#' @description A gene-specific decoder must be provided.
#'
#' @param algorithm  "sga", "sgde", "sgperm", "sge", "sgede", 
#'                   "sgp". Default: "sga".
#'
#' @param method     Method. Default: "DecodeGene". 
#'
#' @returns Decode function for the selected algorithm from the correct package.
#'
#' @family Configuration
#'
#' @examples
#' sgXDecodeGeneFactory(algorithm="sgperm", method="DecodeGene")
#'
#'@importFrom xegaGaGene xegaGaDecodeGene
#'@importFrom xegaGpGene xegaGpDecodeGene
#'@importFrom xegaGeGene xegaGeDecodeGeneFactory
#'@importFrom xegaDfGene xegaDfDecodeGene
#'@importFrom xegaPermGene xegaPermDecodeGene
#'@export
sgXDecodeGeneFactory<-function(algorithm="sga", method="DecodeGene")
{
   if (algorithm=="sga") {f<-xegaGaGene::xegaGaDecodeGene}
   if (algorithm=="sgp") {f<-xegaGpGene::xegaGpDecodeGene}
   if (algorithm=="sge") {f<-xegaGeGene::xegaGeDecodeGeneFactory(method)}
   if (algorithm=="sgede") {f<-xegaGeGene::xegaGeDecodeGeneFactory(method)}
   if (algorithm=="sgde") {f<-xegaDfGene::xegaDfDecodeGene}
   if (algorithm=="sgperm") {f<-xegaPermGene::xegaPermDecodeGene}
if (!exists("f", inherits=FALSE))
        {stop("sgX DecodeGene Factory label ", algorithm, " does not exist")}
return(f)
}
	
