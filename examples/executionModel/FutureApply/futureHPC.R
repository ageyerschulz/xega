library(parallelly)
library(future)
library(future.callr)
library(xega)

oldOptions<-options()
options(width=100)

prtSolution<-function(txt, r)
{
s<-r$solution
cat(txt, ":", s$phenotypeValue,":", s$phenotype, s$fitness, "\n")
}

vecT<-function(rLst, accessFUN)
{
  unlist(lapply(rLst, FUN=accessFUN)) 
}

FutureApplyBenchmarkHPCSeq<-function(
      penv=envXOR, grammar=BG, 
      generations=generations, popsize=popsize,
      crossrate=crossrate, mutrate=mutrate, evalmethod=evalmethod,
      replay=replay, verbose=verbose,
      workers=workers)
{
cat("XOR Sequential (S)\n")
gc(full=TRUE)
d<-xegaRun(penv=penv, grammar=grammar, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate, evalmethod=evalmethod,
       executionModel="Sequential", profile=TRUE,
       verbose=verbose, replay=replay[1])
cat("XOR FutureSequential (FS)\n")
gc(full=TRUE)
plan(sequential)
e<-xegaRun(penv=penv, grammar=grammar, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate, evalmethod=evalmethod,
       executionModel="FutureApply", profile=TRUE,
       verbose=verbose, replay=replay[3])
plan(sequential)
# Compare Solutions
prtSolution("        (S)", d)
prtSolution("       (FS)", e)
rLst<-c(list(d), list(e))
######
# Timings main.
tMain<-vecT(rLst, accessFUN=function(x){x$timer$tMainLoop})
toMainS<-tMain/tMain[1]
tNext<-vecT(rLst, accessFUN=function(x){x$timer$tNextPopulation})
toNextS<-tNext/tNext[1]
tEval<-vecT(rLst, accessFUN=function(x){x$timer$tEvalPopulation})
toEvalS<-tEval/tEval[1]
popSize<-rep(popsize, length(tMain))
worker<-rep(workers, length(tMain))
parModel<-c("S", "FS")
######
df<-data.frame(parModel, popSize, worker, 
      tMain, toMainS, tNext, toNextS,  tEval, toEvalS, 
    row.names=c("(S)", "(FS)"))
colnames(df)<-c("parModel", "pop", "w",
              "tMain (s)", "tMain/(S)",
              "tNext (s)", "tNext/(S)",
              "tEval (s)", "tEval/(S)")
#####
cat("======================================================================", "\n")
cat("XOR", "workers:", workers, "popsize:", popsize, "generations:", generations, 
     "crossrate:", crossrate, "mutrate:", mutrate, "\n")
cat("======================================================================", "\n")
print(df)
cat("======================================================================", "\n\n")
return(df)
}


FutureApplyBenchmarkHPC<-function(
      penv=envXOR, grammar=BG, 
      generations=generations, popsize=popsize,
      crossrate=crossrate, mutrate=mutrate, evalmethod=evalmethod,
      replay=replay, verbose=verbose,
      workers=workers)
{
plan(sequential)
cat("XOR FutureMultiCore (FMC)\n")
gc(full=TRUE)
plan(multicore, workers=workers)
f<-xegaRun(penv=penv, grammar=grammar, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate, evalmethod=evalmethod,
       executionModel="FutureApply", profile=TRUE,
       verbose=verbose, replay=replay[3])
plan(sequential)
cat("XOR FutureCallr (FCallr)\n")
gc(full=TRUE)
plan(callr, workers=workers)
h<-xegaRun(penv=penv, grammar=grammar, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate, evalmethod=evalmethod,
       executionModel="FutureApply", profile=TRUE,
       verbose=verbose, replay=replay[5])
plan(sequential)
# Compare Solutions
prtSolution("      (FMC)", f)
prtSolution("   (FCallr)", h)
rLst<-c(list(f), list(h))
######
# Timings main.
tMain<-vecT(rLst, accessFUN=function(x){x$timer$tMainLoop})
toMainS<-tMain/tMain[1]
tNext<-vecT(rLst, accessFUN=function(x){x$timer$tNextPopulation})
toNextS<-tNext/tNext[1]
tEval<-vecT(rLst, accessFUN=function(x){x$timer$tEvalPopulation})
toEvalS<-tEval/tEval[1]
parModel<-c("FMC", "FCallr")
popSize<-rep(popsize, length(tMain))
worker<-rep(workers, length(tMain))
######
df<-data.frame(parModel, popSize, worker, 
               tMain, toMainS, tNext, toNextS,  tEval, toEvalS, 
    row.names=c("(FMC)", "(FCallr)"))
colnames(df)<-c("parModel", "pop", "w",
              "tMain (s)", "tMain/(S)",
              "tNext (s)", "tNext/(S)",
              "tEval (s)", "tEval/(S)")
#####
cat("======================================================================", "\n")
cat("XOR", "workers:", workers, "popsize:", popsize, "generations:", generations, 
     "crossrate:", crossrate, "mutrate:", mutrate, "\n")
cat("======================================================================", "\n")
print(df)
cat("======================================================================", "\n\n")
return(df)
}

main<-function()
{
#####
verbose<-1
# replay<-sample(1:1000, 8)
replay<-rep(73, 8)
popsize<-c(64, 640, 6400, 64000)
popsize<-c(64, 640, 6400)
generations<-2
crossrate<-0.2
mutrate<-0.2
workers<-8

maxWorkers<-parallelly::availableCores()
#
envXOR<-NewEnvXOR()
BG<-compileBNF(booleanGrammar())

envXOR$name()

cat("FutureApply Benchmark Examples (HPC).\n")

dfFinal<-data.frame()

for (i in (1:length(popsize)))
{
dfp<-data.frame()
dfnew<-FutureApplyBenchmarkHPCSeq(
      penv=envXOR, grammar=BG, 
      generations=generations, popsize=popsize[i],
      crossrate=crossrate, mutrate=mutrate, evalmethod="Deterministic", 
      replay=replay, verbose=verbose,
      workers=1)
dfp<-rbind(dfp, dfnew)

for (w in (1:maxWorkers))
{
dfnew<-FutureApplyBenchmarkHPC(
      penv=envXOR, grammar=BG, 
      generations=generations, popsize=popsize[i],
      crossrate=crossrate, mutrate=mutrate, evalmethod="Deterministic", 
      replay=replay, verbose=verbose,
      workers=w)

dfp<-rbind(dfp, dfnew)
}
dfp[,5]<-dfp[,4]/dfp[1,4]
dfp[,7]<-dfp[,6]/dfp[1,6]
dfp[,9]<-dfp[,8]/dfp[1,8]
dfFinal<-rbind(dfFinal, dfp)
}

saveRDS(dfFinal, file= "HPCout.rds")
}

system.time(main(), gcFirst=TRUE)

options<-oldOptions
