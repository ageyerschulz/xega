

#
# (c) 2026 Andreas Geyer-Schulz
#          Migration: Message receiving (non-blocking).
#          Package: xega
#

#' Receive genes from neighbor processes.
#'
#' @param lF      Local function configuration.
#'
#' @return A gene list. 
#' 
#' @family Migration
#' 
#' @examples 
#' lF<-list()
#' lF$npid<-function() {10}
#' lF$pid<-function()  {3}
#' lF$nrecv<-function() {1}
#' lF$CommunicationTopology<-xegaCommunicationTopologyFactory(method="ring")
#' path<-tempdir()
#' lF$path<- function() {path}
#' genes<-list(sample(0:1, 10, replace=TRUE))
#' rdsSendGenes(genes, lF)
#' fn<-list.files(lF$path())
#' rdsReceiveGenes(lF)
#' lF$pid<-function()  {4}
#' rdsReceiveGenes(lF)
#'@export
rdsReceiveGenes<-function(lF)
{ genes<-list()
pat<-"*\\.rds"
fns<-list.files(path=lF$path(), pattern=pat)
pat2<-paste0("To", lF$pid(), "RND")
fns<-fns[grepl(pat2, fns)]
if (length(fns)==0) {return(genes)}
for (i in (1:length(fns)))
{  fn<-file.path(lF$path(), fns[i])
   g<-readRDS(fn)
   genes<-c(genes, g)
   file.remove(fn)}
return(genes) }


### Documentation missing.
## mpi receive
# receive all pending messages with gene lists (tag=9)


#' Receive genes from neighbor processes (non-blocking).
#'
#' @description Multiple mpi messages are received, if they exist
#'              (non-blocking).  
#'              Test for existence of message: mpi.iprobe()
#'              and message receive: mpi.recv.Robj().  
#'
#' @param lF      Local function configuration.
#'
#' @return A gene list. 
#'
#' @family Migration
#'
### @importFrom Rmpi mpi.any.source
### @importFrom Rmpi mpi.iprobe
### @importFrom Rmpi mpi.recv.Robj
#' @export
mpiReceiveGenes<-function(lF)
{ genes<-list()
### while (Rmpi::mpi.iprobe(source=Rmpi::mpi.any.source(), tag=9, comm=1, status=0))
### { g<-Rmpi::mpi.recv.Robj(source=Rmpi::mpi.any.source(), tag=9, comm=1, status=0)
while (lF$Rmpi$mpi.iprobe(source=lF$Rmpi$mpi.any.source(), tag=9, comm=1, status=0))
{ g<-lF$Rmpi$mpi.recv.Robj(source=lF$Rmpi$mpi.any.source(), tag=9, comm=1, status=0)
genes<-c(genes, g) }
return(genes)
}

#' Receive genes from neighbor processes (blocking).
#'
#' @description Multiple mpi messages are received, if they exist
#'              (blocking).  
#'              Test for existence of message (blocks): mpi.probe() 
#'              and message receive: mpi.recv.Robj().  
#'
#' @param lF      Local function configuration.
#'
#' @return A gene list. 
#'
#' @family Migration
#'
### @importFrom Rmpi mpi.any.source
### @importFrom Rmpi mpi.probe
### @importFrom Rmpi mpi.recv.Robj
#' @export
mpiReceiveGenesBlocking<-function(lF)
{ genes<-list()
### while (Rmpi::mpi.probe(source=Rmpi::mpi.any.source(), tag=9, comm=1, status=0))
### { g<-Rmpi::mpi.recv.Robj(source=Rmpi::mpi.any.source(), tag=9, comm=1, status=0)
cat("Receive ...\n")
if (lF$Rmpi$mpi.probe(source=lF$Rmpi$mpi.any.source(),  tag=9, comm=1, status=0)) {
   while (lF$Rmpi$mpi.iprobe(source=lF$Rmpi$mpi.any.source(), tag=9, comm=1, status=0))
   { g<-lF$Rmpi$mpi.recv.Robj(source=lF$Rmpi$mpi.any.source(), tag=9, comm=1, status=0)
     genes<-c(genes, g) 
     cat("Received Genes. \n") }
}
return(genes)
}

#' Factory for configuring the message receiving
#'
#' Avalailable methods: 
#' \enumerate{
#'  \item "rds": Message receiving via rds-file I/O.
#'  \item "mpi": Message receiving via mpi. Code with comments. Non-blocking.
#'  \item "mpib": Message receiving via mpi. Code with comments. Blocking.
#'  }
#'
#' @param method    Method. Default: "rds".
#'
#' @return A function for sending a message.
#'
#' @family Configuration
#'
#' @examples
#' xegaReceiveFactory(method="rds")
#'
#'@export
xegaReceiveFactory<-function(method="rds")
{
   if (method=="rds") {f<-rdsReceiveGenes}
   if (method=="mpi") {f<-mpiReceiveGenes}
   if (method=="mpib") {f<-mpiReceiveGenesBlocking}
if (!exists("f", inherits=FALSE))
        {stop("xegaReceive Factory label ", method, " does not exist")}
return(f)
}

