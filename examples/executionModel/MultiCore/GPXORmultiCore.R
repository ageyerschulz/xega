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

MultiCoreBenchmark<-function(
      penv=envXOR, grammar=BG, 
      generations=generations, popsize=popsize,
      crossrate=crossrate, mutrate=mutrate, 
      replay=replay, verbose=verbose) 
{
cat("XOR Sequential (S)\n")
gc(full=TRUE)
d<-Run(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate, 
       executionModel="Sequential", profile=TRUE,
       verbose=verbose, replay=replay[1])

cat("XOR MultiCore (MC, cores=NA(",availableCores(),"))\n")

gc(full=TRUE)
e<-Run(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate, 
       executionModel="MultiCore", profile=TRUE,
       verbose=verbose, replay=replay[2])

cat("XOR MultiCore (MC, cores=2)\n")
gc(full=TRUE)
f<-Run(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate, 
       executionModel="MultiCore", cores=2, profile=TRUE,
       verbose=verbose, replay=replay[3])

cat("XOR MultiCore (MC, cores=4)\n")
gc(full=TRUE)
g<-Run(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate, 
       executionModel="MultiCore", cores=4, profile=TRUE,
       verbose=verbose, replay=replay[4])

cat("XOR MultiCore (MC, cores=8)\n")
gc(full=TRUE)
h<-Run(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate, 
       executionModel="MultiCore", cores=8, profile=TRUE,
       verbose=verbose, replay=replay[5])

cat("XOR MultiCore (MC, cores=16)\n")
gc(full=TRUE)
i<-Run(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate, 
       executionModel="MultiCore", cores=16, profile=TRUE,
       verbose=verbose, replay=replay[6])

cat("XOR MultiCore (MC, cores=24)\n")
gc(full=TRUE)
j<-Run(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate, 
       executionModel="MultiCore", cores=24, profile=TRUE,
       verbose=verbose, replay=replay[7])

cat("XOR MultiCore (MC, cores=32)\n")
gc(full=TRUE)
k<-Run(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       crossrate=crossrate, mutrate=mutrate, 
       executionModel="MultiCore", cores=32, profile=TRUE,
       verbose=verbose, replay=replay[8])

cat("=====================================================================", "\n")
prtSolution("            (S)", d)
prtSolution(" (MC, cores=NA)", e)
prtSolution("  (MC, cores=2)", f)
prtSolution("  (MC, cores=4)", g)
prtSolution("  (MC, cores=8)", h)
prtSolution(" (MC, cores=16)", i)
prtSolution(" (MC, cores=24)", j)
prtSolution(" (MC, cores=32)", k)

rLst<-c(list(d), list(e), list(f), list(g), list(h),
        list(i), list(j), list(k))

# Timings main.
tMain<-vecT(rLst, accessFUN=function(x){x$timer$tMainLoop})
toMainS<-tMain/tMain[1]
tNext<-vecT(rLst, accessFUN=function(x){x$timer$tNextPopulation})
toNextS<-tNext/tNext[1]
tEval<-vecT(rLst, accessFUN=function(x){x$timer$tEvalPopulation})
toEvalS<-tEval/tEval[1]

df<-data.frame(tMain, toMainS, tNext, toNextS,  tEval, toEvalS, 
    row.names=c("(S)", 
                "(MC, NA)",  
                "(MC, 2) ",  
                "(MC, 4) ",  
                "(MC, 8) ",  
                "(MC, 16) ",  
                "(MC, 24) ",
                "(MC, 32) "
               ))
colnames(df)<-c(
              "tMain (s)", "tMain/(S)",
              "tNext (s)", "tNext/(S)",
              "tEval (s)", "tEval/(S)")

cat("======================================================================", "\n")
cat("XOR", "popsize:", popsize, "generations:", generations, 
    "crossrate:", crossrate, "mutrate:", mutrate, "\n") 
cat("======================================================================", "\n")
print(df)
cat("======================================================================", "\n\n")
}
####

verbose<-0
# replay<-sample(1:1000, 8)
replay<-rep(73, 8)
popsize<-64
generations<-10
crossrate<-0.3
mutrate<-0.1
#
envXOR<-NewEnvXOR()
BG<-compileBNF(booleanGrammar())

cat("MultiCore Benchmark Examples.\n")

MultiCoreBenchmark(
      penv=envXOR, grammar=BG, 
      generations=generations, popsize=64,
      crossrate=crossrate, mutrate=mutrate, 
      replay=replay, verbose=verbose) 

MultiCoreBenchmark(
      penv=envXOR, grammar=BG, 
      generations=generations, popsize=640,
      crossrate=crossrate, mutrate=mutrate, 
      replay=replay, verbose=verbose) 

MultiCoreBenchmark(
      penv=envXOR, grammar=BG, 
      generations=generations, popsize=6400,
      crossrate=crossrate, mutrate=mutrate, 
      replay=replay, verbose=verbose) 
