
args = commandArgs(trailingOnly=TRUE)

pid=as.integer(args[1])
npid=as.integer(args[2])

library(xega) 

Config<-readRDS(file="TSPlin105Het4Config.rds") 

demeResult<-xegaReRun(Config[[1+pid]], 
                      pid=pid,
                      npid=npid,
                      Send="rds",
                      Receive="rdsb",
                      Configuration=FALSE,
                      AdaptLimit="Id",
                      path="./rdsHetResult")

cat("pid:", pid, "\n")
summary(warnings())

