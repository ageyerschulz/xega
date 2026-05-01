
#
# (c) 2026 Andreas Geyer-Schulz
#          Migration: The migration algorithm.
#          Package: xega
#

#' Generate a local function list to test migration.
#'
#' @family Migration
#' 
#' @importFrom xegaSelectGene parm
#' @importFrom xegaSelectGene SelectGeneFactory 
#' @importFrom xegaPopulation lFxegaGaGene 
#' @importFrom xegaPopulation ApplyFactory 
#'
#' @export
NewLFxegaMigrate<-function()
{ lF<-xegaPopulation::lFxegaGaGene
  lF$Pipeline<-xegaSelectGene::parm("NoPipe")
  lF$lapply<-xegaPopulation::ApplyFactory(method="Sequential")
  lF$path<-xegaSelectGene::parm(tempdir())
######
  lF$Nmigrants<-xegaSelectGene::parm(1)
  lF$pid<-xegaSelectGene::parm(5)
  lF$npid<-xegaSelectGene::parm(10)
  lF$TopK<-xegaSelectGene::parm(1) # for selection method "TopK"
  lF$SelMigrant<-xegaSelectGene::SelectGeneFactory(method="TopK")
  lF$SelReplace<-xegaSelectGene::SelectGeneFactory(method="TopK")
  lF$CommunicationTopology<-xegaCommunicationTopologyFactory(method="ring")
  lF$Send<-xegaSendFactory(method="rds")
  lF$Receive<-xegaReceiveFactory(method="rds")
######
  return(lF) }

#' Migrate genes.
#'
#' @description The migration algorithm performs the following steps:
#'    \enumerate{
#'    \item Select emigrants.
#'    \item Send emigrants to recipients defined by the communication topology.
#'    \item Receive immigrants.
#'    \item Replace some genes by immigrants.
#'    } 
#'     
#' @details The classic non-blocking migration strategy is:
#'    \enumerate{
#'    \item Select the best gene.          
#'    \item Send it to the neigbor (in a ring topology).
#'    \item Receive immigrants from the neighbor (in a ring topology).
#'    \item If there are immigrants, replace the worst genes by the immigrants.  
#'    }
#'  
#' @param population   A population. 
#' @param fit          A fitness vector.
#' @param lF           Local function configuration.
#'
#' @return pop         A list of genes 
#'
#' @family Migration
#'
#' @examples 
#' lF<-NewLFxegaMigrate()
#' p<-xegaPopulation::xegaInitPopulation(10, lF)
#' p1<-xegaPopulation::xegaEvalPopulation(p, lF)
#' population<-p1$pop
#' fit<-p1$fit
#' p2<-xegaMigrate(population, fit, lF)
#' p2fit<-unlist(lapply(p2, function(x) { x$fit }))
#' cat("Mean before:", mean(fit), "after migration:", mean(p2fit), "\n")
#' lF$pid<-xegaSelectGene::parm(6) 
#' p3<-xegaMigrate(population, fit, lF)
#' p3fit<-unlist(lapply(p3, function(x) { x$fit }))
#' cat("Mean before:", mean(p2fit), "after migration:", mean(p3fit), "\n")
#' 
#' @importFrom xegaSelectGene parm
#'@export
xegaMigrate<-function(population, fit, lF)
{ pop<-population
midx<-lF$SelMigrant(fit, lF, size=lF$Nmigrants())
emigrants<-population[midx]
rc<-lF$Send(population[midx], lF)
immigrants<-lF$Receive(lF)
if (length(immigrants)>0)
   {lF$TopK<-xegaSelectGene::parm(length(immigrants))
   ridx<-lF$SelReplace(((-1)*fit), lF, size=length(immigrants))
   pop[ridx]<-immigrants}
return(pop)
}

