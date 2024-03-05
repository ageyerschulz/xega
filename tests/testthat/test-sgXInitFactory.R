library(testthat)
library(xegaGaGene)
library(xegaGpGene)
library(xegaDfGene)
library(xegaPermGene)
library(xega)

test_that("sgXInitGeneFactory sga OK",
 {
 f<-sgXInitGeneFactory(algorithm="sga")
 expect_identical(body(f), body(xegaGaGene::xegaGaInitGene))
}
)

test_that("sgXInitGeneFactory sgp OK",
 {
 f<-sgXInitGeneFactory(algorithm="sgp")
 expect_identical(body(f), body(xegaGpGene::xegaGpInitGene))
}
)

test_that("sgXInitGeneFactory sgde OK",
 {
 f<-sgXInitGeneFactory(algorithm="sgde")
 expect_identical(body(f), body(xegaDfGene::xegaDfInitGene))
}
)

test_that("sgXInitGeneFactory sgperm OK",
 {
 f<-sgXInitGeneFactory(algorithm="sgperm")
 expect_identical(body(f), body(xegaPermGene::xegaPermInitGene))
}
)

test_that("sgXInitGeneFactory sgunknown OK",
 {
 expect_error(
 sgXInitGeneFactory(algorithm="sgunknown"),
 "sgunknown")
}
)
