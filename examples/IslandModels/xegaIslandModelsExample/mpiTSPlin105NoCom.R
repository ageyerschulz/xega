
source("rmpiProfile.R")

suppressPackageStartupMessages(library(xega)) 

Config<-readRDS(file="TSPlin105Het4Config.rds") 

demeResult<-xegaReRun(Config[[1+mpi.comm.rank()]], 
                      AdaptLimit="Id",
                      RmpiFNS=RmpiFNS,
                      pid=mpi.comm.rank(),
                      npid=mpi.comm.size(),
                      migrate="No",
                      Send="mpi",
                      Receive="mpi",
                      migratevery=100,
                      migrationDebug=TRUE,
                      Configuration=FALSE,
                      verbose=2,
                      path="./mpiHetNoCom")

