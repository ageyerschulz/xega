
source("rmpiProfile.R")

suppressPackageStartupMessages(library(xega))

Config<-readRDS(file="TSPlin105HomConfig.rds" ) 

demeResult<-xegaReRun(Config, 
                      mutate="MutateGeneGreedy",
                      lambda=0.01,
		      generations=200, 
                      RmpiFNS=RmpiFNS,
                      pid=mpi.comm.rank(),
                      npid=mpi.comm.size(),
                      Send="mpi",
                      Receive="mpi",
                      Configuration=FALSE,
                      path="./mpiHomResult") 

