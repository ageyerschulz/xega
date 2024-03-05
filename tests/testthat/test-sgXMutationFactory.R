library(testthat)
library(xegaGaGene)
library(xegaGpGene)
library(xegaDfGene)
library(xegaPermGene)
library(xega)

test_that("sgXMutationFactory sga OK",
 {
 a<-sgXMutationFactory(algorithm="sga", method="MutateGene")
 expect_identical(body(a), body(xegaGaGene::xegaGaMutateGene))
}
)

test_that("sgXMutationFactory sgp OK",
 {
 a<-sgXMutationFactory(algorithm="sgp", method="MutateGene")
 expect_identical(body(a), body(xegaGpGene::xegaGpMutateAllGene))
}
)

test_that("sgXMutationFactory sgde OK",
 {
 a<-sgXMutationFactory(algorithm="sgde", method="MutateGeneDE")
 expect_identical(body(a), body(xegaDfGene::xegaDfMutateGeneDE))
}
)

test_that("sgXMutationFactory sgperm OK",
 {
 a<-sgXMutationFactory(algorithm="sgperm", method="MutateGene")
 expect_identical(body(a), body(xegaPermGene::xegaPermMutateGeneOrderBased))
}
)

test_that("sgXMutationFactory sgunknown OK",
 {
 expect_error(
 sgXMutationFactory(algorithm="sgunknown", method="MutateGene"),
 "sgunknown")
}
)
