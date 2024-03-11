
library(xega)

verbose<-1
# replay<-sample(1:1000, 8)
replay<-rep(73, 8)
popsize<-64
generations<-2


envXOR<-NewEnvXOR()
BG<-compileBNF(booleanGrammar())

cat("XOR Sequential (S)\n")

gc(full=TRUE)
c<-Run(penv=envXOR, grammar=BG, algorithm="sgp",
       generations=generations, popsize=popsize,
       executionModel="Sequential", profile=TRUE,
       verbose=verbose, replay=replay[1])

cat("XOR Sequential (S) seqApply \n")

seqApply<-function(pop, EvalGene, lF)
{
   cat("seqApply\n")
   lapply(pop, EvalGene, lF)
}

gc(full=TRUE)
d<-Run(penv=envXOR, grammar=BG, algorithm="sgp",
       generations=generations, popsize=popsize,
       executionModel="Sequential", uParApply=seqApply,  profile=TRUE,
       verbose=verbose, replay=replay[1])

