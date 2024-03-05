library(testthat)
library(xegaGaGene)
library(xegaGpGene)
library(xegaDfGene)
library(xegaPermGene)
library(xega)

test_that("sgXCrossoverFactory sga OK",
 {
 f<-sgXCrossoverFactory(algorithm="sga", method="CrossGene")
 expect_identical(body(f), body(xegaGaGene::xegaGaCrossGene))
}
)

test_that("sgXCrossoverFactory sgp OK",
 {
 f<-sgXCrossoverFactory(algorithm="sgp", method="AllCrossGene")
 expect_identical(body(f), body(xegaGpGene::xegaGpAllCrossGene))
}
)

test_that("sgXCrossoverFactory sgde OK",
 {
 f<-sgXCrossoverFactory(algorithm="sgde", method="CrossGene")
 expect_identical(body(f), body(xegaDfGene::xegaDfCrossGene))
}
)

test_that("sgXCrossoverFactory sgperm OK",
 {
 f<-sgXCrossoverFactory(algorithm="sgperm", method="CrossGene")
 expect_identical(body(f), body(xegaPermGene::xegaPermCrossGene))
}
)

test_that("sgXCrossoverFactory sgunknown OK",
 {
 expect_error(
 sgXCrossoverFactory(algorithm="sgunknown", method="CrossGene"),
 "sgunknown")
}
)
