
#
# (c) 2021 Andreas Geyer-Schulz
#     Simple Genetic Programming in R. V0.1
#     Layer: Gene-Level Functions
#            For gene representation of derivation trees.
#     Package: xegaGpGene
#

#' A constant function with a boolean grammar.
#'
#' @description For the distribution of examples of BNF in grammars.
#'
#' @details Imported from package xegaBNF for use in examples.
#'
#' @return A named list with $filename and  $BNF,
#'         the grammar of a boolean grammar with two variables.
#'
#' @family Grammar
#'
#' @examples
#' booleanGrammar()
#' @importFrom xegaBNF booleanGrammar
#' @export
booleanGrammar<-xegaBNF::booleanGrammar

#' Compile a BNF.
#'
#' @description \code{compileBNF()} produces a context-free grammar
#'               from its specification in Backus-Naur form (BNF).
#'               Warning: No error checking implemented.
#'
#' @details A grammar consists of the symbol table \code{ST}, the production
#'          table \code{PT}, the start symbol \code{Start},
#'          and the short production
#'          table \code{SPT}. An example BNF is provided 
#'          by \code{booleanGrammar()}.
#'
#' The function performs the following steps:
#'  \enumerate{
#'  \item Make the symbol table. 
#'  \item Make the production table.
#'  \item Extract the start symbol.
#'  \item Compile a short production table.
#'  \item Return the grammar.}
#'
#' For a full documentation, see <https://CRAN.R-project.org/package=xegaBNF>
#' 
#' @param g        A character string with a BNF.
#' @param verbose  Boolean. TRUE: Show progress. Default: FALSE.
#'
#' @return A grammar object (list) with the attributes
#'         \code{name} (the filename of the grammar),
#'         \code{ST} (symbol table),
#'         \code{PT} (production table),
#'         \code{Start} (the start symbol of the grammar),
#'         and \code{SPT} (the short production table).
#'
#' @family Grammar
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' g$ST
#' g$PT
#' g$Start
#' g$SPT
#' @importFrom xegaBNF compileBNF
#' @export
compileBNF<-xegaBNF::compileBNF

#' Generate the problem environment EnvXOR
#'
#' @description \code{NewEnvXOR()} generates the problem environment
#'              for the XOR-Problem.
#'
#'  The problem environment provides an abstract interface
#'  to the simple genetic programming algorithm.
#'  \code{ProblemEnv$f(parm)} defines the function we want to optimize.
#'
#'  A problem environment is a function factory with the following
#'  elements:
#'
#'  \enumerate{
#'  \item
#'  \code{name()}:   A string with the name of the environment.
#'  \item
#'  \code{ProblemEnv$f(word)}:
#'   Function with the \code{word} a word of the language (as a text string).
#'  }
#'
#'  Should be provided by the user as a standard R-file.
#'
#' @family Problem Environment
#'
#' @return The problem environment:
#' \itemize{
#' \item \code{$name}: The name of the problem environment.
#' \item \code{$f}:    The fitness function. 
#'                     For this environment, 
#'                     fitness is defined as the number of correct test cases 
#'                     (correct function)
#'                     and the inverse of the number of terminal symbols.
#'                     The second part means that
#'                     a boolean function with a fewer number of variables 
#'                     and logical functions is fitter than one with more 
#'                     variables and logical functions if both solve 
#'                     the same number of test cases. 
#' }
#'
#' @examples
#' EnvXOR<-NewEnvXOR()
#' EnvXOR$name()
#' a2<-"OR(OR(D1, D2), (AND(NOT(D1), NOT(D2))))"
#' a3<-"OR(OR(D1, D2), AND(D1, D2))"
#' a4<-"AND(OR(D1,D2),NOT(AND(D1,D2)))"
#' gp4<-"(AND(AND(OR(D2,D1),NOT(AND(D1,D2))),(OR(D2,D1))))"
#' EnvXOR$f(a2)
#' EnvXOR$f(a3)
#' EnvXOR$f(a4)
#' EnvXOR$f(gp4)
#'
#' @importFrom xegaDerivationTrees treeLeaves
#' @export
NewEnvXOR<-function()
{
penv<-list()
penv[["name"]]<-function() {"EnvXOR"}
penv[["BuildTEST"]]<-function(expr) {
        f<-paste("function(v) {
        AND<-function(x,y){return(x & y)}
        NAND<-function(x,y){return(!(x & y))}
        OR<-function(x,y){return(x|y)}
        NOT<-function(x){return(!x)}
        D1<-v[1]
        D2<-v[2]
        return(", expr, ")}", sep="")
        return(eval(parse(text=f)))
}
penv[["TestCases"]]<-matrix(c(0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0),
                nrow=4, ncol=3, byrow=TRUE)
penv[["f"]]<-function(expr, gene=NULL, lF=NULL)
{ TEST<-penv$BuildTEST(expr)
s<-0
for (i in 1:nrow(penv$TestCases))
{ s<-s+
(penv$TestCases[i,ncol(penv$TestCases)]==TEST(penv$TestCases[i,]))
}
return(s)
#
# if (identical(gene, NULL)) {return(s)}
#  b<-xegaDerivationTrees::treeLeaves(gene$gene1, lF$Grammar$ST)
#  s<-(s+(1/(b^2)))
#return(s)
}
return(penv)
}
