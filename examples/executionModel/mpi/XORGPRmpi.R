#
# Introspection, Timing and Parallel Processing 
# R with Rmpi on a Notebook/PC and on a Slurm Cluster 
# (seqentially and the following cluster topologies 
#               1 Node and n Cores
#               k Nodes and with k times n Cores)
# (c) 2022 Andreas Geyer-Schulz

# Load R profile. 
# Default profile on SLURM:
#source(paste0(Sys.getenv("R_LIBS_SITE"),"/Rmpi/Rprofile"))
source("Rprofile")

#
# Native rmpi
#

# Next line on SLURM KIT Cluster necessary
# source(paste0(Sys.getenv("R_LIBS_SITE"),"/Rmpi/Rprofile"))

# Load the R MPI package if it is not already loaded.
if  (!is.loaded("mpi_initialize")) {
   library("Rmpi")
}

# Start

# 
# startSlaves<-function()
# {
#	nc<-mpi.universe.size()-1
#	mpi.spawn.Rslaves(nslaves=nc)
#	return(nc)
#}
#

#
# The starting of slaves is taken care of in the Rprofile
#
startSlaves<-function(){
   return(mpi.comm.size(0))}

# Stop
stopSlaves<-function()
{
Rmpi::mpi.close.Rslaves(dellog=FALSE)
Rmpi::mpi.exit()
}

# Stop and quit R
stopSlavesQuitR<-function()
{
Rmpi::mpi.close.Rslaves(dellog=FALSE)
Rmpi::mpi.quit()
}

rmpiLapply<-function(pop, EvalGene, lF)
{
   Rmpi::mpi.parLapply(pop, FUN=EvalGene, lF=lF)
}

main<-function()
{
nSlaves<-startSlaves()
cat("Number of Slaves:", nSlaves, "\n")
gc(full=TRUE)

cat("XOR GP Rmpi::mpi.parLapply() \n")

verbose<-1
# replay<-sample(1:1000, 8)
replay<-rep(73, 8)
popsize<-640
generations<-10

envXOR<-NewEnvXOR()
BG<-compileBNF(booleanGrammar())

d<-Run(penv=envXOR, grammar=BG, algorithm="sgp",
       generations=generations, popsize=popsize,
       executionModel="Sequential", uParApply=rmpiLapply,  profile=TRUE,
       verbose=verbose, replay=replay[1], batch=TRUE)

}

system.time(main())

message("Program finished.")

stopSlavesQuitR()
