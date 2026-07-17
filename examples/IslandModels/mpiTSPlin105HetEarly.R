
source("rmpiProfile.R")

suppressPackageStartupMessages(library(xega))

Config<-readRDS(file="TSPlin105Het4EarlyConfig.rds") 

demeResult<-xegaReRun(Config[[1+mpi.comm.rank()]], 
                      generations=1000,
                      RmpiFNS=RmpiFNS,
                      pid=mpi.comm.rank(),
                      npid=mpi.comm.size(),
                      Send="mpi",
                      Receive="mpi",
                      Configuration=FALSE,
                      path="./mpiHetResult")

