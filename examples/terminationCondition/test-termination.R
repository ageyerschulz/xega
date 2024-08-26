
library(testthat)
library(smoof)
library(xega)
source("smoofWrapper.R")

penvAckley<-smoofWrapperFactory(makeAckleyFunction(2))

test_that("penvAckley, max=FALSE, terminationCondition=AbsoluteError, terminationEps=0.2",
{
cat("smoof: penvAckley, max=FALSE, terminationCondition=AbsoluteError,terminationEps=0.2\n")
a<-xegaRun(penv=penvAckley, max=FALSE, generations=1000, popsize=500, 
           terminationCondition="AbsoluteError", 
           terminationEps=0.2, verbose=0)
cat("Optimum:",a$solution$phenotypeValue, 
    "Stopped at generation", nrow(a$popStat)," \n")
expect_lt(nrow(a$popStat), 1000)
expect_lt(a$solution$phenotypeValue, 0.2)
expect_gt(a$solution$phenotypeValue, -0.2)
}
)

test_that("penvAckley, terminationCondition=AbsoluteError, terminationEps=0.1",
{
cat("smoof: penvAckley, max=FALSE, terminationCondition=AbsoluteError,terminationEps=0.1\n")
a<-xegaRun(penv=penvAckley, max=FALSE, generations=1000, popsize=500, 
           terminationCondition="AbsoluteError", 
           terminationEps=0.1, verbose=0)
cat("Optimum:",a$solution$phenotypeValue, 
    "Stopped at generation", nrow(a$popStat)," \n")
expect_lt(nrow(a$popStat), 1000)
expect_lt(a$solution$phenotypeValue, 0.1)
expect_gt(a$solution$phenotypeValue, -0.1)
}
)

test_that("penvAckley, terminationCondition=RelativeError, terminationEps=0.1",
{
cat("smoof: penvAckley, max=FALSE, terminationCondition=RelativeError,terminationEps=0.1\n")
a<-xegaRun(penv=penvAckley, max=FALSE, generations=1000, popsize=100, 
           terminationCondition="RelativeError", 
           terminationEps=0.1, verbose=0)
cat("Optimum:",a$solution$phenotypeValue, 
    "Stopped at generation", nrow(a$popStat)," \n")
expect_equal(nrow(a$popStat), 1001)
expect_lt(a$solution$phenotypeValue, 0.1)
expect_gt(a$solution$phenotypeValue, -0.1)
}
)

cat("xega::Parabola2D\n")

test_that("Parabola2D, max=FALSE",
{
cat("Parabola2D, max=FALSE\n")
a<-xegaRun(penv=Parabola2D, max=FALSE, generations=1000, popsize=100, 
           verbose=0)
cat("Optimum:",a$solution$phenotypeValue, 
    "Stopped at generation", nrow(a$popStat)," \n")
expect_equal(nrow(a$popStat), 1001)
expect_lt(a$solution$phenotypeValue, 0.1)
expect_gt(a$solution$phenotypeValue, -0.1)
}
)

test_that("Parabola2D, max=FALSE, terminationCondition=RelativeError, terminationEps=0.1",
{
cat("Parabola2D: max=FALSE, terminationCondition=AbsoluteError, terminationEps=0.1 \n")
a<-xegaRun(penv=Parabola2D, max=FALSE, generations=1000, popsize=100, 
           terminationCondition="AbsoluteError", 
           terminationEps=0.1, verbose=0)
cat("Optimum:",a$solution$phenotypeValue, 
    "Stopped at generation", nrow(a$popStat)," \n")
expect_lt(nrow(a$popStat), 1001)
expect_lt(a$solution$phenotypeValue, 0.1)
expect_gt(a$solution$phenotypeValue, -0.1)
}
)

test_that("Parabola2D, max=TRUE, terminationCondition=RelativeError, terminationEps=0.1",
{
cat("xega::Parabola2D: Max=TRUE, terminationCondition=AbsoluteError, terminationEps=0.1 \n")
a<-xegaRun(penv=Parabola2D, max=TRUE, generations=1000, popsize=100, 
           terminationCondition="AbsoluteError", 
           terminationEps=0.1, verbose=0)
cat("Optimum:",a$solution$phenotypeValue, 
    "Stopped at generation", nrow(a$popStat)," \n")
expect_lt(nrow(a$popStat), 1001)
expect_lt(a$solution$phenotypeValue, 40.6)
expect_gt(a$solution$phenotypeValue, 40.4)
}
)

test_that("Parabola2D, max=FALSE, terminationCondition=RelativeError, terminationEps=0.1",
{
cat("xega::Parabola2D:max=FALSE, terminationCondition=RelativeError, terminationEps=0.1 \n")
a<-xegaRun(penv=Parabola2D, max=FALSE, generations=1000, popsize=100, 
           terminationCondition="RelativeError", 
           terminationEps=0.1, verbose=0)
cat("Optimum:",a$solution$phenotypeValue, 
    "Stopped at generation", nrow(a$popStat)," \n")
expect_equal(nrow(a$popStat), 1001)
expect_lt(a$solution$phenotypeValue, 0.1)
expect_gt(a$solution$phenotypeValue, -0.1)
}
)

test_that("Parabola2D, max=TRUE, terminationCondition=RelativeError, terminationEps=0.1",
{
cat("xega::Parabola2D, Max=TRUE, terminationCondition=RelativeError, terminationEps=0.1 \n")
a<-xegaRun(penv=Parabola2D, max=TRUE, generations=1000, popsize=100, 
           terminationCondition="RelativeError", 
           terminationEps=0.01, verbose=0)
cat("Optimum:",a$solution$phenotypeValue, 
    "Stopped at generation", nrow(a$popStat)," \n")
expect_lt(nrow(a$popStat), 1001)
expect_lt(a$solution$phenotypeValue, 40.905)
expect_gt(a$solution$phenotypeValue, 40.095)
}
)

test_that("Parabola2D, max=FALSE, terminationCondition=RelativeErrorZero, terminationEps=0.1",
{
cat("xega::Parabola2D:max=FALSE, terminationCondition=RelativeErrorZero, terminationEps=0.1 \n")
a<-xegaRun(penv=Parabola2D, max=FALSE, generations=1000, popsize=100, 
           terminationCondition="RelativeErrorZero", 
           terminationEps=0.1, verbose=0)
cat("Optimum:",a$solution$phenotypeValue, 
    "Stopped at generation", nrow(a$popStat)," \n")
expect_lt(nrow(a$popStat), 1001)
expect_lt(a$solution$phenotypeValue, 0.1)
expect_gt(a$solution$phenotypeValue, -0.1)
}
)

test_that("Parabola2D, max=FALSE, terminationCondition=RelativeErrorZero, terminationEps=0.01",
{
cat("xega::Parabola2D:max=FALSE, terminationCondition=RelativeErrorZero, terminationEps=0.01 \n")
a<-xegaRun(penv=Parabola2D, max=FALSE, generations=1000, popsize=100, 
           terminationCondition="RelativeErrorZero", 
           terminationEps=0.01, verbose=0)
cat("Optimum:",a$solution$phenotypeValue, 
    "Stopped at generation", nrow(a$popStat)," \n")
expect_lt(nrow(a$popStat), 1001)
expect_lt(a$solution$phenotypeValue, 0.1)
expect_gt(a$solution$phenotypeValue, -0.1)
}
)

Parabol2D<-list()
Parabol2D$name<-Parabola2D$name
Parabol2D$bitlength<-Parabola2D$bitlength
Parabol2D$genelength<-Parabola2D$genelength
Parabol2D$lb<-Parabola2D$lb
Parabol2D$ub<-Parabola2D$ub
Parabol2D$f<-Parabola2D$f



test_that("Parabol2D, max=FALSE",
{
cat("xega::Parabol2D, max=FALSE, \n")

a<-xegaRun(penv=Parabol2D, max=FALSE, generations=1000, popsize=100, 
           verbose=0)
cat("Optimum:",a$solution$phenotypeValue, 
    "Stopped at generation", nrow(a$popStat)," \n")
expect_equal(nrow(a$popStat), 1001)
expect_lt(a$solution$phenotypeValue, 0.1)
expect_gt(a$solution$phenotypeValue, -0.1)
}
)

test_that("Parabol2D, max=FALSE, terminationCondition=AbsoluteError, terminationEps=0.1",
{
cat("xega::Parabol2D, max=FALSE, terminationCondition=AbsoluteError, terminationEps=0.1 \n")
expect_error(a<-xegaRun(penv=Parabol2D, max=FALSE, generations=1000, popsize=100, 
 terminationCondition="AbsoluteError", terminationEps=0.1, verbose=0))
cat("Raised Error!\n")
}
)

test_that("Parabola2DEarly, max=FALSE",
{
cat("xega::Parabola2DEarly, max=FALSE \n")
a<-xegaRun(penv=Parabola2DEarly, max=FALSE, generations=1000, popsize=100, 
           verbose=0)
cat("Optimum:",a$solution$phenotypeValue, 
    "Stopped at generation", nrow(a$popStat)," \n")
expect_equal(nrow(a$popStat), 1001)
expect_lt(a$solution$phenotypeValue, 0.1)
expect_gt(a$solution$phenotypeValue, -0.1)
}
)

test_that("Parabola2DEarly, max=FALSE, early=TRUE",
{
cat("xega::Parabola2DEarly, max=FALSE, early=TRUE \n")
a<-xegaRun(penv=Parabola2DEarly, max=FALSE, generations=1000, popsize=100, 
           early=TRUE, terminationEps=0.1, verbose=0)
cat("Optimum:",a$solution$phenotypeValue, 
    "Stopped at generation", nrow(a$popStat)," \n")
expect_lt(nrow(a$popStat), 1001)
expect_lt(a$solution$phenotypeValue, 0.1)
expect_gt(a$solution$phenotypeValue, -0.1)
}
)

