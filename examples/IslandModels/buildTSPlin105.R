
#
# xegaRun configurations for TSP lin105 for island models.
#

library(TSP)
library(xegaSelectGene)
library(xega) 

distance<-function( coord, i, j)
{ xd<-coord[i, 1]-coord[j,1]
  yd<-coord[i, 2]-coord[j,2]
  rij<-sqrt(xd*xd +yd*yd)
return(rij)
}

x<-read_TSPLIB("lin105.tsp")
k<-as.matrix(x)

y<-matrix(0, nrow=nrow(k), ncol=nrow(k))

for (i in 1:nrow(k)) {
   for (j in 1:nrow(k)) {
      y[i, j] <-distance(k, i, j) }}

STSPlin105<-newTSP(y, Name="STSPlin105", Cities=NA, Solution=14379)

deme0<-xegaRun(STSPlin105,
    max=FALSE,
    algorithm="sgperm",
    popsize=200,
    generations=200,
    crossrate=0.05,
    mutrate=0.5,
    elitist=TRUE,
    evalmethod="Deterministic",
    reportEvalErrors=TRUE,
    genemap="Identity",
    crossover="CrossGene",
    max2opt=20,
    lambda=0.05,
    mutation="MutateGeneMix",
    replication="Kid1PipelineG",
    initgene="InitGene",
    selection="SUS",
    mateselection="SUS",
    verbose=2,
    cores=2,
    pipeline="PipeG",
    executionModel="MultiCore",
    profile=TRUE,
    batch=TRUE,
    migrate=TRUE,
    migrateEvery=2,
    pid=0, 
    npid=4, 
    Send="rds",
    Receive="rds",
    CommunicationTopology="random",
    AdaptLimit="Id",
    Configuration=TRUE)

deme1<-xegaReRun(deme0, 
    mutation="MutateGene2Opt", max2opt=20, pid=0)

deme2<-xegaReRun(deme0, 
    mutation="MutateGenekOptLK", max2opt=200, pid=1)

deme3<-xegaReRun(deme0, 
    mutation="MutateGenekInversion",
    pid=2)

deme4<-xegaReRun(deme0, 
    mutation="MutateGeneGreedy", pid=3)

deme5<-xegaReRun(deme0, 
    mutation="MutateGeneGreedy", pid=3,
    terminationCondition="LEQ", terminationThreshold=16000)

demeConfig1<-list(deme1, deme2, deme3, deme4)

demeConfig2<-list(deme1, deme2, deme3, deme5)

# Same configuration n-times.
saveRDS(deme0, "TSPlin105HomConfig.rds")

# 4 different configurations.
saveRDS(demeConfig1, "TSPlin105Het4Config.rds")

# 4 different configurations. Early Termination
saveRDS(demeConfig2, "TSPlin105Het4EarlyConfig.rds")

