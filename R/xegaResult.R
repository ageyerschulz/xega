#' Writes xega results after each iteration to a rds file.
#'
#' @details The file \code{xegaAnyTimeResult.rds} is overwritten 
#'          after each generation.
#'          The function is intended to be used with the defaults
#'          (\code{xegaAnyTimeResult()}).
#' 
#' @param pp               Population.
#' @param ft               Fitness vector.       
#' @param lF                Local function configuration.
#' @param allsolutions      Boolean.       
#' @param popStat           Population statistics
#'                          (mean, min, Q1, median, Q3, max, var, mad).
#' @param evalFail          Number of evaluation failures.
#' @param mlT               Main loop timer function.
#' @param GAconfiguration   Configuration of the genetic algorithm.
#' @param path              File path.   
#' 
#' @return Invisible 0.
#'
#' @importFrom xegaPopulation xegaBestInPopulation
#' @export
xegaAnyTimeResult<-function(mlT, 
                     pp, ft, lF, allsolutions, 
                     popStat, evalFail, 
                     GAconfiguration, path)
{

   timer=list()
   timer$tMainLoop<-mlT("TimeUsed")   
   rc<-xegaBestInPopulation(pp, ft, lF, allsolutions)

   popS<-matrix(popStat, byrow=TRUE, ncol=8)
   colnames(popS)<-c("mean", "min", "Q1", "median", "Q3", "max", "var", "mad")

   result<-list(popStat=popS,
                    fit=ft,
                    solution=rc,
                    evalFail=evalFail,
                    GAconfig=list(GAconfiguration$GAconf),
                    GAenv=GAconfiguration$GAenv,
                    timer=timer,
                    logfn=NA,
                    resfn=NA)

   fn1<-"tmp0AnyTimeResult.rds"
   fn2<-"xegaAnyTimeResult.rds"
   pfn1<-file.path(path, fn1)    
   pfn2<-file.path(path, fn2)    
   saveRDS(object=result, file=pfn1)
   if (!file.rename(pfn1, pfn2)) 
      {stop("file.rename in xegaAnyTimeResult failed.")}
   invisible(0)
}
