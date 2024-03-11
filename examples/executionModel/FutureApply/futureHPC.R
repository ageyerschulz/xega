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

FutureApplyBenchmarkHPCSeq<-function(
      penv=envXOR, grammar=BG, 
      generations=generations, popsize=popsize,
      crossrate=crossrate, mutrate=mutrate, evalmethod=evalmethod,
      replay=replay, verbose=verbose,
      workers=workers)
{
cat("XOR Sequential (S)\n")
gc(full=TRUE)
d<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate, evalmethod=evalmethod,
       executionModel="Sequential", profile=TRUE,
       verbose=verbose, replay=replay[1])
cat("XOR FutureSequential (FS)\n")
gc(full=TRUE)
plan(sequential)
e<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
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
######
df<-data.frame(tMain, toMainS, tNext, toNextS,  tEval, toEvalS, 
    row.names=c("(S)", "(FS)"))
colnames(df)<-c(
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
f<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate, evalmethod=evalmethod,
       executionModel="FutureApply", profile=TRUE,
       verbose=verbose, replay=replay[3])
plan(sequential)
cat("XOR FutureCallr (FCallr)\n")
gc(full=TRUE)
plan(callr, workers=workers)
h<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
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
######
df<-data.frame(tMain, toMainS, tNext, toNextS,  tEval, toEvalS, 
    row.names=c("(FMC)", "(FCallr)"))
colnames(df)<-c(
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

#####
verbose<-1
# replay<-sample(1:1000, 8)
replay<-rep(73, 8)
popsize<-c(64, 640, 6400, 64000)
popsize<-c(64, 640)
generations<-3
crossrate<-0.2
mutrate<-0.2
workers<-8

maxWorkers<-parallelly::availableCores()
#
envXOR<-NewEnvXOR()
BG<-compileBNF(booleanGrammar())

cat("FutureApply Benchmark Examples (HPC).\n")

dfFinal<-FutureApplyBenchmarkHPCSeq(
      penv=envXOR, grammar=BG, 
      generations=generations, popsize=popsize[i],
      crossrate=crossrate, mutrate=mutrate, evalmethod="Deterministic", 
      replay=replay, verbose=verbose,
      workers=w)

for (w in (1:maxWorkers))
{
for (i in (1:length(popsize)))
{
dfnew<-FutureApplyBenchmarkHPC(
      penv=envXOR, grammar=BG, 
      generations=generations, popsize=popsize[i],
      crossrate=crossrate, mutrate=mutrate, evalmethod="Deterministic", 
      replay=replay, verbose=verbose,
      workers=w)

dfFinal<-rbind(dfFinal, dfnew)
}
}

saveRDS(dfFinal, file= "HPCout.rds")

