library(testthat)
library(xegaGaGene)
library(xegaGpGene)
library(xegaDfGene)
library(xegaPermGene)
library(xega)

test_that("sgXDecodeGeneFactory sga OK",
 {
 f<-sgXDecodeGeneFactory(algorithm="sga")
 expect_identical(body(f), body(xegaGaGene::xegaGaDecodeGene))
}
)

test_that("sgXDecodeGeneFactory sgp OK",
 {
 f<-sgXDecodeGeneFactory(algorithm="sgp")
 expect_identical(body(f), body(xegaGpGene::xegaGpDecodeGene))
}
)

test_that("sgXDecodeGeneFactory sgde OK",
 {
 f<-sgXDecodeGeneFactory(algorithm="sgde")
 expect_identical(body(f), body(xegaDfGene::xegaDfDecodeGene))
}
)

test_that("sgXDecodeGeneFactory sgperm OK",
 {
 f<-sgXDecodeGeneFactory(algorithm="sgperm")
 expect_identical(body(f), body(xegaPermGene::xegaPermDecodeGene))
}
)

test_that("sgXDecodeGeneFactory sgunknown OK",
 {
 expect_error(
 sgXDecodeGeneFactory(algorithm="sgunknown"),
 "sgunknown")
}
)


