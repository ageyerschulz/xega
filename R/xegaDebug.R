
#' Debug (show) progress in xegaRun main loop.
#'
#' @param debug Boolean. Default: \code{FALSE}.
#'              If \code{TRUE}, show \code{msg} and \code{obj}.
#' @param msg   Text message.
#' @param obj   R object.
#'
#' @return 0 (invisible)
#' 
#' @examples
#' xegaDebug(debug=FALSE, msg="nothing is shown")
#' xegaDebug(debug=TRUE, msg="nothing is shown")
#' xegaDebug(debug=TRUE, msg="xegaDebug:", obj=xegaDebug)
#' @export
xegaDebug<-function(debug, msg="debug message", obj=NULL)
{
if (debug) 
  { cat(msg, "\n")
  if (!is.null(obj)) {print(obj)}
  }
invisible(0)
}
