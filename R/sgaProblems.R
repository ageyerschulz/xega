#
# (c) 2021 Andreas Geyer-Schulz
#          Simple Genetic Algorithm in R. V 0.1
#          Layer: Top-level main programs.     
#          Package: xega
#

#' Problem environment for a 2-dimensional quadratic parabola
#'
#' @description Problem environment for finding maxima and minima
#'              of a 2-dimensional quadratic parabola.
#'
#' @return A named list 
#'      \itemize{
#'      \item \code{$name()}: Returns the name of the problem environment.
#'      \item \code{$bitlength()}: The vector of the 
#'                                 bitlengths of the parameters.  
#'      \item \code{$genelength()}: The number of bits of a gene.  
#'      \item \code{$lb()}:         The vector of lower bounds 
#'                                 of the parameters. 
#'      \item \code{$ub()}:  The vector of upper bounds of the parameters. 
#'      \item \code{$f(parm)}:   The implementation of the function of the
#'                           quadratic parabola. 
#'            \itemize{
#'                    \item \code{parm}:  A 2-element vector of reals.
#'                    \item Returns the value of the function.
#'                    }
#'      \item \code{$describe()}:   Returns the description of 
#'                          the problem environment.
#'      \item \code{$solution()}:   The solutions (maxima/minima) of the 
#'                          problem environment (if known). 
#'       }
#'
#' @family Problem Environment
#'
#' @examples
#' names(Parabola2D)
#' Parabola2D$name()
#' Parabola2D$describe()
#' Parabola2D$bitlength()
#' Parabola2D$genelength()
#' Parabola2D$lb()
#' Parabola2D$ub()
#' Parabola2D$f
#' Parabola2D$f(c(2.2, -1.37))
#' Parabola2D$solution()
#' Parabola2D$solution()$minimum
#' Parabola2D$solution()$minpoints
#' Parabola2D$solution()$maximum
#' Parabola2D$solution()$maxpoints
#' @importFrom xegaSelectGene Parabola2DFactory
#' @export
Parabola2D<-xegaSelectGene::Parabola2DFactory()

#' Problem environment for a 2-dimensional quadratic parabola.
#'
#' @description An example of a problem environment with an 
#'              early termination condition.
#'
#' @return A problem environment (see \link{Parabola2D}).
#'         \code{Parabola2DEarly$terminate(solution, lF)} 
#'         is a test function which returns true if the \code{solution} 
#'         is in an epsilon environment of a known solution. 
#'         To invoke this function, use \code{xegaRun( ..., early=TRUE, ...)}.
#'         The epsilon which determines 
#'         the length of the interval as a fraction
#'         of the known optimal solution is configured by  
#'         e.g. \code{xegaRun( ..., terminationEps=0.001, ...)}.
#'
#' @family Problem Environment
#'
#' @importFrom xegaSelectGene Parabola2DEarlyFactory
#' @export
Parabola2DEarly<-xegaSelectGene::Parabola2DEarlyFactory()

