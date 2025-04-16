library(testthat)
library(xegaGpGene)
library(xega)

test_that("NewEnvXOR OK",
 {
 e<-NewEnvXOR()
 g<-xegaGpInitGene(lFxegaGpGene)
 dg<-lFxegaGpGene$DecodeGene(g, lFxegaGpGene)
 expect_identical(e$name(), "EnvXOR")
 expect_equal(e$f("OR(OR(D1, D2), (AND(NOT(D1), NOT(D2))))"), 2)
 expect_equal(e$f("OR(OR(D1, D2), AND(D1, D2))"), 3)
 expect_equal(e$f("AND(OR(D1,D2),NOT(AND(D1,D2)))"), 4)
# expect_gt(e$f(dg, g), e$f(dg))
}
)

