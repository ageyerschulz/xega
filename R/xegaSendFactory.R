

#
# (c) 2026 Andreas Geyer-Schulz
#          Migration: Message sending.
#          Package: xega
#

#' Construct a unique file name with sender and receiver embedded in name.
#'
#' @param from  Integer (pid of message sender).
#' @param to    Integer (pid of message receiver).
#' @param path  File path. Default: ".".
#'
#' @return A file name of the form From<spid>To<rpid>RND<pad>.rds
#' 
#' @family Migration
#'
#' @examples
#' rdsFileName(3, 2)
#'
#'@export
rdsFileName<-function(from, to, path=".")
{pad<-paste(stats::runif(1))
 pad<-substr(pad, 3, nchar(pad))
fn<-paste0("From", from, "To", to, "RND", pad, ".rds")
return(file.path(path, fn)) }

#' Send genes to neighbor process.
#'
#' @description Uses saveRDS for writing messages to files.
#' The file name is of the form From<spid>To<rpid>RND<pad>.rds
#'
#' @param genes   A gene list.
#' @param lF      Local function configuration.
#'
#' @return 0 (invisible) 
#' 
#' @family Migration
#' 
#' @examples 
#' lF<-list()
#' lF$npid<-function() {10}
#' lF$pid<-function()  {3}
#' lF$nrecv<-function() {1}
#' lF$CommunicationTopology<-xegaCommunicationTopologyFactory(method="random")
#' path<-tempdir()
#' lF$path<- function() {path}
#' genes<-list(sample(0:1, 10, replace=TRUE))
#' print(genes)
#' rdsSendGenes(genes, lF)
#' fn<-list.files(lF$path(), pattern="*\\.rds")
#' print(fn)
#' readRDS(file.path(lF$path(), fn))
#'@importFrom stats runif
#'@export
rdsSendGenes<-function(genes, lF)
{ dest<-lF$CommunicationTopology(lF)
for (i in (1:length(dest)))
{ fn<-rdsFileName(lF$pid(), dest[1], path=lF$path())
  saveRDS(object=genes, file=fn) }
invisible(0)
}

## Description missing
## mpi send


#' Send genes to neighbor process.
#'
#' @description Uses Rmpi::mpi.send.Robj().
#'
#' @param genes   A gene list.
#' @param lF      Local function configuration.
#'
#' @return 0 (invisible) 
#'
#' @family Migration
#'
### @importFrom Rmpi mpi.send.Robj
#' @export
mpiSendGenes<-function(genes, lF)
{
# tag=9 9 is integer for gene lists.
dest<-lF$CommunicationTopology(lF)
for (i in (1:length(dest)))
### { Rmpi::mpi.send.Robj(obj=genes, dest=dest[i], tag=9, comm=1) }
{ lF$RmpiFNS$mpi.send.Robj(obj=genes, dest=dest[i], tag=9, comm=1) }
invisible(0)
}

#' Factory for configuring the message sending
#'
#' Avalailable methods: 
#' \enumerate{
#'  \item "rds": Message sending via rds-file I/O.
#'  \item "mpi": Message senging via mpi. Code with comments.
#'  }
#'
#' @param method    Method. Default: "rds".
#'
#' @return A function for sending a message.
#'
#' @family Configuration
#'
#' @examples
#' xegaSendFactory(method="rds")
#'
#'@export
xegaSendFactory<-function(method="rds")
{
   if (method=="rds") {f<-rdsSendGenes}
   if (method=="mpi") {f<-mpiSendGenes}
if (!exists("f", inherits=FALSE))
        {stop("xegaSend Factory label ", method, " does not exist")}
return(f)
}
