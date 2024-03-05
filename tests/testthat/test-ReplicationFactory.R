library(testthat)
library(xegaGaGene)
library(xegaGpGene)
library(xegaDfGene)
library(xegaPermGene)
library(xega)

test_that("sgXReplicationFactory sga OK",
 {
 f<-sgXReplicationFactory(algorithm="sga", method="Kid1")
 expect_identical(body(f), body(xegaGaGene::xegaGaReplicateGene))
}
)

test_that("sgXReplicationFactory sgp OK",
 {
 f<-sgXReplicationFactory(algorithm="sgp", method="Kid1")
 expect_identical(body(f), body(xegaGaGene::xegaGaReplicateGene))
}
)

test_that("sgXReplicationFactory sgde OK",
 {
 f<-sgXReplicationFactory(algorithm="sgde", method="DE")
 expect_identical(body(f), body(xegaDfGene::xegaDfReplicateGeneDE))
}
)

test_that("sgXReplicationFactory sgperm OK",
 {
 f<-sgXReplicationFactory(algorithm="sgperm", method="Kid1")
 expect_identical(body(f), body(xegaGaGene::xegaGaReplicateGene))
}
)

test_that("sgXReplicationFactory sgunknown OK",
 {
 expect_error(
 sgXReplicationFactory(algorithm="sgunknown", method="DE"),
 "sgunknown")
}
)
