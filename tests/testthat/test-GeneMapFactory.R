library(testthat)
library(xegaGaGene)
library(xegaGpGene)
library(xegaDfGene)
library(xegaPermGene)
library(xega)

test_that("sgXGeneMapFactory sga OK",
 {
 f<-sgXGeneMapFactory(algorithm="sga", method="Bin2Dec")
 expect_identical(body(f), body(xegaGaGene::xegaGaGeneMap))
}
)

test_that("sgXGeneMapFactory sgde OK",
 {
 f<-sgXGeneMapFactory(algorithm="sgde", method="Identity")
 expect_identical(body(f), body(xegaDfGene::xegaDfGeneMapIdentity))
}
)

test_that("sgXGeneMapFactory sgp OK",
 {
 f<-sgXGeneMapFactory(algorithm="sgp", method="Identity")
 expect_identical(body(f), body(xegaGaGene::xegaGaGeneMapIdentity))
}
)

test_that("sgXGeneMapFactory sgperm OK",
 {
 f<-sgXGeneMapFactory(algorithm="sgperm", method="Identity")
 expect_identical(body(f), body(xegaDfGene::xegaDfGeneMapIdentity))
}
)

test_that("sgXGeneMapFactory sgunknown OK",
 {
 expect_error(
 sgXGeneMapFactory(algorithm="sgunknown", method="Identity"),
 "sgunknown")
}
)
