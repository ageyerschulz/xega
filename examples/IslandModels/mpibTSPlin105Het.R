
source("rmpiProfile.R")

suppressPackageStartupMessages(library(xega))

Config<-readRDS(file="TSPlin105Het4Config.rds") 

demeResult<-xegaReRun(Config[[1+mpi.comm.rank()]], 
                      RmpiFNS=RmpiFNS,
                      pid=mpi.comm.rank(),
                      npid=mpi.comm.size(),
                      Send="mpi",
                      Receive="mpib",
                      Configuration=FALSE,
                      debug=FALSE,
                      path="./mpiHetResult")

