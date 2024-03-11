library(parallelly)
library(future)
library(future.callr)
library(xega)

verbose<-1
# replay<-sample(1:1000, 8)
replay<-rep(73, 8)
popsize<-640
generations<-5 
crossrate<-0.25
mutrate<-0.10
workers<-4

envXOR<-NewEnvXOR()
BG<-compileBNF(booleanGrammar())

prtSolution<-function(txt, r)
{
s<-r$solution
cat(txt, ":", s$phenotypeValue,":", s$phenotype, s$fitness, "\n")
}

vecT<-function(rLst, accessFUN)
{
  unlist(lapply(rLst, FUN=accessFUN)) 
}

cat("XOR FutureCluster (FCluster)\n")
gc(full=TRUE)

### Version 1 autoStopCluster + garbage collect cluster object
wcl<-autoStopCluster(makeClusterPSOCK(rep("localhost", workers)))
plan(cluster, workers=wcl)
i<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate,
       executionModel="FutureApply", profile=TRUE,
       evalmethod="Deterministic",
       verbose=verbose, replay=replay[6])
oldplan<-plan(sequential)
oldwcl<-wcl
rm(list="wcl")
gc(full=TRUE)

### Version 2 anonymous function + on.exit(stopCluster(cl))
(function()
{ wcl<-makeClusterPSOCK(rep("localhost", workers))
on.exit(parallel::stopCluster(wcl))
plan(cluster, workers=wcl)
i<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate,
       executionModel="FutureApply", profile=TRUE,
       evalmethod="Deterministic",
       verbose=verbose, replay=replay[6])
oldplan<-plan(sequential)
})()

### Version 3 anonymous function + on.exit(killNode(cl))
(function()
{ wcl<-makeClusterPSOCK(rep("localhost", workers))
on.exit(killNode(wcl))
plan(cluster, workers=wcl)
i<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate,
       executionModel="FutureApply", profile=TRUE,
       evalmethod="Deterministic",
       verbose=verbose, replay=replay[6])
oldplan<-plan(sequential)
})()

