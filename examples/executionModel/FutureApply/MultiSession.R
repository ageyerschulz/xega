library(parallelly)
library(future)
library(future.callr)
library(xega)

prtSolution<-function(txt, r)
{
s<-r$solution
cat(txt, ":", s$phenotypeValue,":", s$phenotype, s$fitness, "\n")
}

#####
verbose<-1
# replay<-sample(1:1000, 8)
replay<-rep(73, 8)
popsize<-64
generations<-2
crossrate<-0.3
mutrate<-0.10
workers<-4
#
envXOR<-NewEnvXOR()
BG<-compileBNF(booleanGrammar())

cat("XOR FutureMultiSession (FMS)\n")
gc(full=TRUE)
plan(multisession, workers=workers)
g<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate,
       executionModel="FutureApply", profile=TRUE,
       evalmethod="Deterministic",
       verbose=verbose, replay=replay[4])
plan(sequential)

prtSolution("      (FMS)", g)

cat("tMain (s):", g$timer$MainLoop, 
    "tNext (s):", g$timer$tNextPopulation, 
     "tEval (s):", g$timer$tEvalPopulation,
     "\n")
 
