
#
# xegaRun configurations for TSP lin105 for island models.
#

suppressPackageStartupMessages(library(TSP))
suppressPackageStartupMessages(library(xegaSelectGene))
suppressPackageStartupMessages(library(xega))

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
    popsize=100,
    generations=100,
    crossrate=0.1,
    mutrate=0.6,
    elitist=TRUE,
    evalmethod="Deterministic",
    reportEvalErrors=TRUE,
    genemap="Identity",
    crossover="CrossGene",
    max2opt=10,
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
    migrate="OnImprovement",
    migrateEvery=1,
    migrationDebug=TRUE,
    pid=0, 
    npid=4, 
    Send="rds",
    Receive="rds",
    CommunicationTopology="random",
    AdaptLimit="Id",
    Configuration=TRUE)

# Same configuration 
saveRDS(deme0, "TSPlin105HomConfig.rds")

