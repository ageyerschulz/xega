library(parallelly)
library(future)
library(future.callr)
library(xega)

prtSolution<-function(txt, r)
{
s<-r$solution
cat(txt, ":", s$phenotypeValue,":", s$phenotype, s$fitness, "\n")
}

vecT<-function(rLst, accessFUN)
{
  unlist(lapply(rLst, FUN=accessFUN)) 
}

FutureApplyBenchmarkNB<-function(
      penv=envXOR, grammar=BG, 
      generations=generations, popsize=popsize,
      crossrate=crossrate, mutrate=mutrate,
      replay=replay, verbose=verbose,
      workers=workers)
{
cat("XOR Sequential (S)\n")
gc(full=TRUE)
d<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate,
       executionModel="Sequential", profile=TRUE,
       evalmethod="Deterministic",
       verbose=verbose, replay=replay[1])

cat("XOR FutureSequential (FS)\n")
gc(full=TRUE)
plan(sequential)
e<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate,
       executionModel="FutureApply", profile=TRUE,
       evalmethod="Deterministic",
       verbose=verbose, replay=replay[3])
plan(sequential)

cat("XOR FutureMultiCore (FMC)\n")
gc(full=TRUE)
plan(multicore, workers=workers)
f<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate,
       executionModel="FutureApply", profile=TRUE,
       evalmethod="Deterministic",
       verbose=verbose, replay=replay[3])
plan(sequential)

## This does not work!
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

cat("XOR FutureCallr (FCallr)\n")
gc(full=TRUE)
plan(callr, workers=workers)
h<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate,
       executionModel="FutureApply", profile=TRUE,
       evalmethod="Deterministic",
       verbose=verbose, replay=replay[5])
plan(sequential)

cat("XOR FutureCluster (FCluster)\n")
gc(full=TRUE)

wcl<-makeClusterPSOCK(rep("localhost", workers))
on.exit(parallel::stopCluster(wcl))
plan(cluster, workers=wcl)
i<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate,
       executionModel="FutureApply", profile=TRUE,
       evalmethod="Deterministic",
       verbose=verbose, replay=replay[6])
plan(sequential)

# Compare Solutions

prtSolution("        (S)", d)
prtSolution("       (FS)", e)
prtSolution("      (FMC)", f)
prtSolution("      (FMS)", g)
prtSolution("   (FCallr)", h)
prtSolution("(FCcluster)", i)

rLst<-c(list(d), list(e), list(f), list(g), list(h), list(i))

# Timings main.
tMain<-vecT(rLst, accessFUN=function(x){x$timer$tMainLoop})
toMainS<-tMain/tMain[1]
tNext<-vecT(rLst, accessFUN=function(x){x$timer$tNextPopulation})
toNextS<-tNext/tNext[1]
tEval<-vecT(rLst, accessFUN=function(x){x$timer$tEvalPopulation})
toEvalS<-tEval/tEval[1]

df<-data.frame(tMain, toMainS, tNext, toNextS,  tEval, toEvalS, 
    row.names=c("(S)", "(FS)", "(FMC)", "(FMS)", "(FCallr)", "(FCluster)"))
colnames(df)<-c(
              "tMain (s)", "tMain/(S)",
              "tNext (s)", "tNext/(S)",
              "tEval (s)", "tEval/(S)")

cat("======================================================================", "\n")
cat("XOR", "workers:", workers, "popsize:", popsize, "generations:", generations, 
     "crossrate:", crossrate, "mutrate:", mutrate, "\n")
cat("======================================================================", "\n")
print(df)
cat("======================================================================", "\n\n")
return(wcl)
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

cat("FutureApply Benchmark Examples (Notebook).\n")

wcl<-FutureApplyBenchmarkNB(
      penv=envXOR, grammar=BG, 
      generations=generations, popsize=popsize,
      crossrate=crossrate, mutrate=mutrate,
      replay=replay, verbose=verbose,
      workers=workers)
parallelly::killNode(wcl)

wcl<-FutureApplyBenchmarkNB(
      penv=envXOR, grammar=BG, 
      generations=generations, popsize=640,
      crossrate=crossrate, mutrate=mutrate,
      replay=replay, verbose=verbose,
      workers=workers)
parallelly::killNode(wcl)

wcl<-FutureApplyBenchmarkNB(
      penv=envXOR, grammar=BG, 
      generations=generations, popsize=6400,
      crossrate=crossrate, mutrate=mutrate,
      replay=replay, verbose=verbose,
      workers=workers)
parallelly::killNode(wcl)

wcl<-FutureApplyBenchmarkNB(
      penv=envXOR, grammar=BG, 
      generations=generations, popsize=popsize,
      crossrate=crossrate, mutrate=mutrate,
      replay=replay, verbose=verbose,
      workers=8)
parallelly::killNode(wcl)

wcl<-FutureApplyBenchmarkNB(
      penv=envXOR, grammar=BG, 
      generations=generations, popsize=640,
      crossrate=crossrate, mutrate=mutrate,
      replay=replay, verbose=verbose,
      workers=8)
parallelly::killNode(wcl)

wcl<-FutureApplyBenchmarkNB(
      penv=envXOR, grammar=BG, 
      generations=generations, popsize=6400,
      crossrate=crossrate, mutrate=mutrate,
      replay=replay, verbose=verbose,
      workers=8)
parallelly::killNode(wcl)

wcl<-FutureApplyBenchmarkNB(
      penv=envXOR, grammar=BG, 
      generations=generations, popsize=64000,
      crossrate=crossrate, mutrate=mutrate,
      replay=replay, verbose=verbose,
      workers=8)
parallelly::killNode(wcl)

wcl<-FutureApplyBenchmarkNB(
      penv=envXOR, grammar=BG, 
      generations=generations, popsize=6400,
      crossrate=crossrate, mutrate=mutrate,
      replay=replay, verbose=verbose,
      workers=16)
parallelly::killNode(wcl)
