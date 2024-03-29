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

PsockApplyBenchmarkLAN<-function(
      penv=envXOR, grammar=BG, 
      generations=generations, popsize=popsize,
      crossrate=crossrate, mutrate=mutrate,
      replay=replay, verbose=verbose,
      workers=workers, names=names)
{
cat("XOR Sequential (S, Small)\n")
gc(full=TRUE)
d<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate,
       executionModel="Sequential", profile=TRUE,
       evalmethod="Deterministic",
       verbose=verbose, replay=replay[1])

cat("XOR Cluster (Small)\n")
gc(full=TRUE)
wcl<-parallelly::makeClusterPSOCK(
     workers=names)
on.exit(parallel::stopCluster(wcl))

e<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate,
       executionModel="Cluster", Cluster=wcl, profile=TRUE,
       evalmethod="Deterministic",
       verbose=0, replay=replay[2])

cat("XOR Cluster (Small)\n")
f<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate,
       executionModel="Cluster", Cluster=wcl, profile=TRUE,
       evalmethod="Deterministic",
       verbose=0, replay=replay[3])

cat("XOR Cluster (Small)\n")
g<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate,
       executionModel="Cluster", Cluster=wcl, profile=TRUE,
       evalmethod="Deterministic",
       verbose=0, replay=replay[4])

cat("XOR Cluster (Small)\n")
h<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=0.1,
       executionModel="Cluster", Cluster=wcl, profile=TRUE,
       evalmethod="Deterministic",
       verbose=0, replay=replay[2])

cat("XOR Cluster (Small)\n")
i<-xegaRun(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=0.1,
       executionModel="Cluster", Cluster=wcl, profile=TRUE,
       evalmethod="Deterministic",
       verbose=verbose, replay=replay[3])
# Compare Solutions

prtSolution("            (S)", d)
prtSolution("(Cluster Small)", e)
prtSolution("(Cluster Small)", f)
prtSolution("(Cluster Small)", g)
prtSolution("(Cluster Small)", h)
prtSolution("(Cluster Small)", i)

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
               "(CSmall 1)", 
               "(CSmall 2)", 
               "(CSmall 3)", 
               "(CSmall 4)",
               "(CSmall 5)")) 
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
# replay<-rep(73, 8)
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
         "em-pop.iism.kit.edu", 
         "em-pop.iism.kit.edu", 
         "em-pop.iism.kit.edu") 

# names<-c("em-ags-nb1.iism.kit.edu",
#         "em-ags-nb1.iism.kit.edu")

cat("Cluster Benchmark Examples Small, Random Seeds (LAN).\n")

wcl<-PsockApplyBenchmarkLAN(
      penv=envXOR, grammar=BG, 
      generations=generations, popsize=popsize,
      crossrate=crossrate, mutrate=mutrate,
      replay=replay, verbose=verbose,
      workers=workers, names=names)

