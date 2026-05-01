
#
# (c) 2026 Andreas Geyer-Schulz
#          Migration between algorithms: Communication Topology
#          Package: xega
#

#' Select neighbor in ring topology.
#'
#' @param  lF  Local function condiguration. 
#'             Required element are 
#'             \itemize{ 
#'             \item \code{lF$npid()}:   Total number of processes.
#'             \item \code{lF$pid()}:    Process number of message sender.
#'             }
#' 
#' @return Process number of message receiver. 
#' 
#' @family Migration 
#' 
#' @examples
#' lF<-list()
#' lF$npid<-function() {10}
#' lF$pid<-function()  {3}
#' ringTop(lF)
#' lF$pid<-function()  {9}
#' ringTop(lF)
#'
#' @export
ringTop<-function(lF)
{ dest<-(lF$pid()+1) %% lF$npid()
  return(dest)}

#' Select a random neighbor from all neighbors. 
#'
#' @param  lF  Local function condiguration. 
#'             Required element are 
#'             \itemize{ 
#'             \item \code{lF$npid()}:   Total number of processes.
#'             \item \code{lF$pid()}:    Process number of message sender.
#'             \item \code{lF$nrecv()}: Number of message receivers.   
#'             }
#' 
#' @return Process number(s) of message receiver(s). 
#' 
#' @family Migration 
#' 
#' @examples
#' lF<-list()
#' lF$npid<-function() {10}
#' lF$pid<-function()  {3}
#' lF$nrecv<-function() {1}
#' rndTop(lF)
#' lF$nrecv<-function() {2}
#' rndTop(lF)
#'
#' @export
rndTop<-function(lF)
{ dest<-sample(0:(lF$npid()-1), lF$nrecv())
  while (lF$pid() %in% dest) { dest<-sample(0:(lF$npid()-1), lF$nrecv())}
  return(dest)}

#' Factory for configuring the communication topology.
#'
#' Avalailable methods: 
#' \enumerate{
#'  \item "random": Returns a function which selects (a) random message receiver(s).
#'  \item "ring": Returns a function which selects the ring neighbour mod(i+1, n)  of node i 
#'                  as message receiver.
#'  }
#'
#' @param method    Method. Default: "random".
#'
#' @return A function containing the communication topology for migration.
#'
#' @family Configuration
#'
#' @examples
#' xegaCommunicationTopologyFactory(method="random")
#'
#'@export
xegaCommunicationTopologyFactory<-function(method="random")
{
   if (method=="random") {f<-rndTop}
   if (method=="ring") {f<-ringTop}
if (!exists("f", inherits=FALSE))
        {stop("xegaCommunicationTopology Factory label ", method, " does not exist")}
return(f)
}

