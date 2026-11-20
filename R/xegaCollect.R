
#
# xegaCollect.R
# (c) 2026 Andreas Geyer-Schulz
#

#' Collects results of all island processes (rds) in a directory.
#' 
#' @description Island processes write their result as rds-file 
#'              into a directory (with the \code{xegaRun}-options  
#'              \code{batch=TRUE} and \code{collect=FALSE}.  
#'              \code{xegaCollect()} reads these files and returns 
#'              an island result object (a list of \code{xegaRun}-results.
#'
#' @param path  The path to the directory with the rds-files. 
#'              Default: "./".
#'
#' @return A named list 
#'         \itemize{
#'         \item \code{$results}: A list of xegaRun result objects. 
#'         \item \code{$rc}: Return code. 
#'         }
#'
#'@export
xegaCollect<-function(path="./")
{
rc<-0
maxDelay=5
result=list()
r<-list()
pat<-"*\\.rds"
pat2<-"xegaResult"
s<-0.01
j<-1
filesDone<-list()
repeat
{ fnsall<-list.files(path=path, pattern=pat)
  fns<-fnsall[grepl(pat2, fnsall)]
if (!length(fns)==0)
{
for (i in (1: length(fns)))
{  fname<-fns[[i]]
   if (fname %in% filesDone) {next}
   fn<-file.path(path, fname)
   readError<-FALSE
   tryCatch(
            {g<-readRDS(fn)},
             error=function(e)
                     {# cat("rdsCollect: readRDS", fn, "failed.\n")
                       Sys.sleep(0.1)
                       readError<<-TRUE
                     }
           )
   if (readError) {break}
   r[[length(r)+1]]<-g
   filesDone<-c(filesDone, fname)
#   file.remove(fn) 
}}
   # Sys.sleep(s)
   j<-j+1
if ((j*s)>maxDelay)
   {rc<-(-1); break}
}
res<-list()
res$results<-r
res$rc<-rc
return(res)}

