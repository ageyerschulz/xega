
source("rmpiProfile.R")

library(xega) 

Config<-readRDS(file="TSPlin105HomConfig.rds" ) 

demeResult<-xegaReRun(Config, 
                      RmpiFNS=RmpiFNS,
                      pid=mpi.comm.rank(),
                      npid=mpi.comm.size(),
                      Send="mpi",
                      Receive="mpi",
                      AdaptLimit="Slowest",
                      Configuration=FALSE,
                      path="./mpiHomResult") 

