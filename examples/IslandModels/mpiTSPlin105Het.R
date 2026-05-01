
source("rmpiProfile.R")

library(xega) 

Config<-readRDS(file="TSPlin105Het4Config.rds") 

demeResult<-xegaReRun(Config[[1+mpi.comm.rank()]], 
                      RmpiFNS=RmpiFNS,
                      pid=mpi.comm.rank(),
                      npid=mpi.comm.size(),
                      Send="mpi",
                      Receive="mpi",
                      Configuration=FALSE,
                      path="./mpiHetResult")

