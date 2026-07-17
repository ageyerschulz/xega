
args = commandArgs(trailingOnly=TRUE)

pid=as.integer(args[1])
npid=as.integer(args[2])

suppressPackageStartupMessages(library(xega))

Config<-readRDS(file="TSPlin105Het4EarlyConfig.rds") 

demeResult<-xegaReRun(Config[[1+pid]], 
                      generations=1000,
                      pid=pid,
                      npid=npid,
                      Send="rds",
                      Receive="rds",
                      Configuration=FALSE,
                      path="./rdsHetResult")
