
# A user defined sequential apply function
seqApply<-function(pop, EvalGene, lF)
{
   cat("seqApply\n")
   lapply(pop, EvalGene, lF)
}

# A user defined parallel apply function
rmpiLapply<-function(pop, EvalGene, lF)
{
   Rmpi::mpi.parLapply(pop, FUN=EvalGene, lF=lF)
}

