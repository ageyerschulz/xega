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

FutureApplyBenchmarkLAN<-function(
      penv=envXOR, grammar=BG, 
      generations=generations, popsize=popsize,
      crossrate=crossrate, mutrate=mutrate,
      replay=replay, verbose=verbose,
      workers=workers, names=names)
{
cat("XOR Sequential (S, pop=6400)\n")
gc(full=TRUE)
d<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=6400, 
       crossrate=crossrate, mutrate=mutrate,
       executionModel="Sequential", profile=TRUE,
       evalmethod="Deterministic",
       verbose=verbose, replay=replay[1])

cat("XOR Cluster (pop=64)\n")
gc(full=TRUE)
wcl<-parallelly::makeClusterPSOCK(
     workers=names)
on.exit(parallel::stopCluster(wcl))

e<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=64, 
       crossrate=crossrate, mutrate=mutrate,
       executionModel="Cluster", Cluster=wcl, profile=TRUE,
       evalmethod="Deterministic",
       verbose=verbose, replay=replay[2])

cat("XOR Cluster (pop=640)\n")
f<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=640, 
       crossrate=crossrate, mutrate=mutrate,
       executionModel="Cluster", Cluster=wcl, profile=TRUE,
       evalmethod="Deterministic",
       verbose=verbose, replay=replay[3])

cat("XOR Cluster (pop=6400)\n")
g<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=6400, 
       crossrate=crossrate, mutrate=mutrate,
       executionModel="Cluster", Cluster=wcl, profile=TRUE,
       evalmethod="Deterministic",
       verbose=verbose, replay=replay[4])

cat("XOR Cluster (pop=12800)\n")
h<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=12800, 
       crossrate=crossrate, mutrate=0.1,
       executionModel="Cluster", Cluster=wcl, profile=TRUE,
       evalmethod="Deterministic",
       verbose=verbose, replay=replay[5])

cat("XOR Cluster (mutrate=1.0, 6400)\n")
i<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=6400, 
       crossrate=crossrate, mutrate=0.4,
       executionModel="Cluster", Cluster=wcl, profile=TRUE,
       evalmethod="Deterministic",
       verbose=verbose, replay=replay[6])
# Compare Solutions

prtSolution("               (S)", d)
prtSolution("(C m,     p=64   )", e)
prtSolution("(C m,     p=640  )", f)
prtSolution("(C m,     p=6400 )", g)
prtSolution("(C m=0.2, p=12800)", h)
prtSolution("(C m=1.0, p=6400 )", i)

rLst<-c(list(d), list(e), list(f), list(g), list(h), list(i))

# Timings main.
tMain<-vecT(rLst, accessFUN=function(x){x$timer$tMainLoop})
toMainS<-tMain/tMain[1]
tNext<-vecT(rLst, accessFUN=function(x){x$timer$tNextPopulation})
toNextS<-tNext/tNext[1]
tEval<-vecT(rLst, accessFUN=function(x){x$timer$tEvalPopulation})
toEvalS<-tEval/tEval[1]

df<-data.frame(tMain, toMainS, tNext, toNextS,  tEval, toEvalS, 
    row.names=c("(S)", 
               "(C       64)", 
               "(C      640)", 
               "(C     6400)", 
               "(C.2m 12800)",
               "(C1.m  6400)")) 
colnames(df)<-c(
              "tMain (s)", "tMain/(S)",
              "tNext (s)", "tNext/(S)",
              "tEval (s)", "tEval/(S)")

cat("======================================================================", "\n")
cat("XOR", "workers:", names, "\n")
cat("XOR", "popsize:", popsize, "generations:", generations, 
     "crossrate:", crossrate, "mutrate:", mutrate, "\n")
cat("======================================================================", "\n")
print(df)
cat("======================================================================", "\n\n")
return(df)
}

#####
verbose<-1
replay<-sample(1:1000, 8)
#replay<-rep(73, 8)
popsize<-640
generations<-10
crossrate<-0.3
mutrate<-0.3
workers<-4
#####

envXOR<-NewEnvXOR()
BG<-compileBNF(booleanGrammar())

names<-c("em-pop.iism.kit.edu",
             "em-pop.iism.kit.edu", 
             "em-pop.iism.kit.edu", 
             "em-pop.iism.kit.edu")

workers<-length(names)

cat("Cluster Benchmark Examples (LAN).\n")

results<-FutureApplyBenchmarkLAN(
      penv=envXOR, grammar=BG, 
      generations=generations, popsize=popsize,
      crossrate=crossrate, mutrate=mutrate,
      replay=replay, verbose=verbose,
      workers=workers, names=names)

