
library(testthat)
library(xega)

cat("xega::Parabola2D\n")

Parabola2DRnd1<-Parabola2D
Parabola2DRnd1$f<-function(parm, gene=0, lF=0) {rnorm(1)+sum(parm^{2}) }

Parabola2DRnd4<-Parabola2D
Parabola2DRnd4$f<-function(parm, gene=0, lF=0) {rnorm(1, sd=2)+sum(parm^{2})}

test_that("Parabola2DRnd1, max=FALSE, evalrep=10",
{cat("xega::Parabola2DRnd1, max=FALSE, evalrep=10 \n")
a<-xegaRun(penv=Parabola2DRnd1, max=FALSE, generations=100, popsize=100, 
           evalrep=10, verbose=0)
cat("Optimum:",a$solution$genotype$fit, 
    "\n E(fit):", a$solution$genotype$fit,
    "Var(fit):", a$solution$genotype$var,
    "Std(fit):", a$solution$genotype$sigma,
    "\n Time (s):", a$timer$tMainLoop,
    "Stopped at generation", nrow(a$popStat)," \n")

expect_equal(nrow(a$popStat), 101)
}
)

test_that("Parabola2DRnd1, max=FALSE, evalrep=100, executionModel=MultiCore",
{cat(
 "xega::Parabola2DRnd1, max=FALSE, evalrep=100, executionModel=MultiCore\n")
a<-xegaRun(penv=Parabola2DRnd1, max=FALSE, generations=100, popsize=100, 
           evalrep=100, verbose=0, executionModel="MultiCore")
cat("Optimum:",a$solution$genotype$fit, 
    "\n E(fit):", a$solution$genotype$fit,
    "Var(fit):", a$solution$genotype$var,
    "Std(fit):", a$solution$genotype$sigma,
    "\n Time (s):", a$timer$tMainLoop,
    "Stopped at generation", nrow(a$popStat)," \n")
expect_equal(nrow(a$popStat), 101)
}
)

test_that("Parabola2DRnd1,early,max=FALSE,evalrep=100,executionModel=MultiCore",
{
cat(
 "xega::Parabola2DRnd1, max=FALSE, evalrep=200, executionModel=MultiCore, \n")
cat("popsize=100,terminationCondition=AbsoluteError, terminationEps=0.1 \n")
a<-xegaRun(penv=Parabola2DRnd1, max=FALSE, generations=100, popsize=100, 
           terminationCondition="AbsoluteError", terminationEps=0.1,
           evalrep=200, verbose=0, executionModel="MultiCore")
cat("Optimum:",a$solution$genotype$fit, 
    "\n E(fit):", a$solution$genotype$fit,
    "Var(fit):", a$solution$genotype$var,
    "Std(fit):", a$solution$genotype$sigma,
    "\n Time (s):", a$timer$tMainLoop,
    "Stopped at generation", nrow(a$popStat)," \n")
expect_lt(nrow(a$popStat), 101)
}
)

test_that("Parabola2DRnd1, min, small, evalrep=10, executionModel=MultiCoreHet",
{cat(
 "xega::Parabola2DRnd1, max=FALSE, evalrep=10, executionModel=MultiCoreHet\n")
cat("popsize=10, generations=10. MultiCoreHet has a lot of overhead \n")
a<-xegaRun(penv=Parabola2DRnd1, max=FALSE, generations=10, popsize=10, 
           evalrep=10, verbose=0, executionModel="MultiCoreHet")
cat("Optimum:",a$solution$genotype$fit, 
    "\n E(fit):", a$solution$genotype$fit,
    "Var(fit):", a$solution$genotype$var,
    "Std(fit):", a$solution$genotype$sigma,
    "\n Time (s):", a$timer$tMainLoop,
    "Stopped at generation", nrow(a$popStat)," \n")
expect_equal(nrow(a$popStat), 11)
}
)

