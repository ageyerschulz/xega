#
# Introspection, Timing and Parallel Processing 
# R with Rmpi on a Notebook/PC and on a Slurm Cluster 
# (seqentially and the following cluster topologies 
#               1 Node and n Cores
#               k Nodes and with k times n Cores)
# (c) 2022 Andreas Geyer-Schulz

library(xega)

main<-function()
{
gc(full=TRUE)

cat("XOR GP Notebook Sequential\n")

verbose<-1
# replay<-sample(1:1000, 8)
replay<-rep(73, 8)
popsize<-40000
generations<-10

envXOR<-NewEnvXOR()
BG<-compileBNF(booleanGrammar())

d<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",
       generations=generations, popsize=popsize,
       executionModel="Sequential", profile=TRUE,
       verbose=verbose, replay=replay[1], batch=TRUE)

}

system.time(main())

message("Program finished.")

