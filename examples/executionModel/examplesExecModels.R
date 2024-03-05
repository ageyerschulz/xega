library(future)
library(future.callr)
library(xega)

verbose<-1
# replay<-sample(1:1000, 5)
replay<-rep(73, 5)
popsize<-64
generations<-50

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

cat("XOR Sequential (S)\n")

gc(full=TRUE)
d<-Run(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       executionModel="Sequential", profile=TRUE,
       verbose=verbose, replay=replay[1])

cat("XOR MultiCore (MC)\n")

gc(full=TRUE)
e<-Run(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       executionModel="MultiCore", profile=TRUE,
       verbose=verbose, replay=replay[2])

cat("XOR FutureMultiCore (FMC)\n")


gc(full=TRUE)

# plan(multicore, workers=16)

f<-Run(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       executionModel="FutureMultiCore", profile=TRUE,
       verbose=verbose, replay=replay[3])

# plan(sequential)

cat("XOR FutureMultiSession (FMS)\n")

gc(full=TRUE)

# plan(multisession, workers=16)

g<-Run(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       executionModel="FutureMultiSession", profile=TRUE,
       verbose=verbose, replay=replay[4])

# plan(sequential)

cat("XOR FutureCallr (FC)\n")

gc(full=TRUE)

# plan(callr, workers=16)

h<-Run(penv=envXOR, grammar=BG, algorithm="sgp",  
       generations=generations, popsize=popsize, 
       executionModel="FutureCallr", profile=TRUE,
       verbose=verbose, replay=replay[5])

# plan(sequential)

# Compare Solutions

prtSolution("  (S)", d)
prtSolution(" (MC)", e)
prtSolution("(FMC)", f)
prtSolution("(FMS)", g)
prtSolution(" (FC)", h)

rLst<-c(list(d), list(e), list(f), list(g), list(h))

# Timings main.
tMain<-vecT(rLst, accessFUN=function(x){x$timer$tMainLoop})
toMainS<-tMain/tMain[1]
tNext<-vecT(rLst, accessFUN=function(x){x$timer$tNextPopulation})
toNextS<-tNext/tNext[1]
tEval<-vecT(rLst, accessFUN=function(x){x$timer$tEvalPopulation})
toEvalS<-tEval/tEval[1]

df<-data.frame(tMain, toMainS, tNext, toNextS,  tEval, toEvalS, 
    row.names=c("(S)", "(MC)", "(FMC)", "(FMS)", "(FC)"))

print(df)

