library(testthat)
library(xega)

test_that("RunGA  OK",
 {
 a<-Run(Parabola2D, max=FALSE, verbose=1)
 expect_identical(names(a), 
  c("popStat", "fit", "solution", "evalFail", "GAconfig", "GAenv", "timer"))
          }
)

test_that("RunGA  OK",
 {
 a<-Run(Parabola2DEarly, generations=100, popsize=500, max=FALSE, verbose=0)
 expect_identical(names(a), 
  c("popStat", "fit", "solution", "evalFail", "GAconfig", "GAenv", "timer"))
          }
)

test_that("RunGA  OK",
 {
 a<-Run(Parabola2DEarly, generations=100, popsize=500, max=FALSE, 
	   scaling="ThresholdScaling", verbose=0)
 expect_identical(names(a), 
  c("popStat", "fit", "solution", "evalFail", "GAconfig", "GAenv", "timer"))
          }
)

test_that("RunGA max=FALSE, profile=TRUE, batch=FALSE  OK",
 {
 a<-Run(Parabola2D, max=FALSE, verbose=0, profile=TRUE, batch=FALSE)
 expect_identical(names(a), 
  c("popStat", "fit", "solution", "evalFail", "GAconfig", "GAenv", "timer"))
 unlink("xegaResult*.rds")
          }
)

test_that("RunGA max=FALSE, profile=TRUE, batch=TRUE  OK",
 {
skip_on_cran()
 a<-Run(Parabola2D, max=FALSE, verbose=0, profile=TRUE, batch=TRUE)
 expect_identical(names(a), 
  c("popStat", "fit", "solution", "evalFail", "GAconfig", "GAenv", "timer"))
 unlink("xegaResult*.rds")
          }
)

test_that("RunGA, replay,   OK",
 {
 a<-Run(Parabola2D, replay=5, verbose=0)
 b<-Run(Parabola2D, replay=5, verbose=0)
 c<-Run(Parabola2D, replay=7, verbose=0)
 expect_equal(a$solution$fitness, b$solution$fitness) 
 expect_equal(a$solution$fitness==c$solution$fitness, FALSE) 
 expect_equal(c$solution$fitness==b$solution$fitness, FALSE) 
          }
)

test_that("ReRun, OK",
 {
 a<-Run(Parabola2D, replay=5, verbose=0)
 b<-ReRun(a)
 expect_equal(a$solution$fitness, b$solution$fitness) 
          }
)


test_that("xegaVersion OK",
          {
	   expect_output(xegaVersion())
          }
)

