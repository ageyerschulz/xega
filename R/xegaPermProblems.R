
#' The problem environment lau15
#'
#' @description
#' 15 abstract cities for which a traveling salesman solution is sought.
#' Solution: A path with a length of 291.
#'
#' @references
#' Lau, H. T. (1986):
#' \emph{Combinatorial Heuristic Algorithms in FORTRAN}.
#' Springer, 1986.
#' <doi:10.1007/978-3-642-61649-5>
#'
#' @family Problem Environment
#'
#' @examples 
#' names(lau15)
#' lau15$genelength()
#' @importFrom xegaSelectGene lau15
#' @export
lau15<-xegaSelectGene::lau15
