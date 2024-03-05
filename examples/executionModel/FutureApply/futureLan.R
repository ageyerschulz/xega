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
      workers=workers, names)
{
cat("XOR Sequential (S)\n")
gc(full=TRUE)
d<-Run(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate,
       executionModel="Sequential", profile=TRUE,
       evalmethod="Deterministic",
       verbose=verbose, replay=replay[1])

cat("XOR FutureCluster (FCluster)\n")
gc(full=TRUE)

wcl<-parallelly::makeClusterPSOCK(
     workers=names,
     master="em-ags-nb1.iism.kit.edu",
     port=10250)
on.exit(parallel::stopCluster(wcl))
plan(cluster, workers=wcl)
i<-Run(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate,
       executionModel="FutureApply", profile=TRUE,
       evalmethod="Deterministic",
       verbose=verbose, replay=replay[6])
plan(sequential)

# Compare Solutions

prtSolution("        (S)", d)
prtSolution("(FCcluster)", i)

rLst<-c(list(d), list(i))

# Timings main.
tMain<-vecT(rLst, accessFUN=function(x){x$timer$tMainLoop})
toMainS<-tMain/tMain[1]
tNext<-vecT(rLst, accessFUN=function(x){x$timer$tNextPopulation})
toNextS<-tNext/tNext[1]
tEval<-vecT(rLst, accessFUN=function(x){x$timer$tEvalPopulation})
toEvalS<-tEval/tEval[1]

df<-data.frame(tMain, toMainS, tNext, toNextS,  tEval, toEvalS, 
    row.names=c("(S)", "(FCluster)"))
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

# names<-c("em-pop.iism.kit.edu",
#             "em-pop.iism.kit.edu", 
#             "em-folk.iism.kit.edu", 
#             "em-ags-nb1.iism.kit.edu") 

names<-c("em-pop.iism.kit.edu",
         "em-pop.iism.kit.edu") 

# names<-c("em-ags-nb1.iism.kit.edu",
#         "em-ags-nb1.iism.kit.edu")

cat("FutureApply Benchmark Examples (LAN).\n")

wcl<-FutureApplyBenchmarkLAN(
      penv=envXOR, grammar=BG, 
      generations=generations, popsize=popsize,
      crossrate=crossrate, mutrate=mutrate,
      replay=replay, verbose=verbose,
      workers=workers, names=names)

