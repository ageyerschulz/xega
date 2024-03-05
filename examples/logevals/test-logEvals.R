
library(xega)

a<-Run(penv=Parabola2D, generations=10, popsize=20, verbose=0, logevals=TRUE)

a1<-Run(penv=Parabola2D, max=FALSE, generations=10, popsize=20, verbose=0, 
    logevals=TRUE, path="/home/dj2333/dev/cran/xega/examples/logevals/logs/")

b<-Run(penv=Parabola2D, algorithm="sgde", generations=10, popsize=20, 
       mutation="MutateGeneDE", scalefactor="Uniform", crossover="UCrossGene",
       genemap="Identity", replication="DE",
       selection="UniformP", mateselection="UniformP", accept="Best",
       max=FALSE, verbose=0, logevals=TRUE)

envXOR<-NewEnvXOR()
BG<-compileBNF(booleanGrammar())

d<-Run(penv=envXOR, grammar=BG, algorithm="sgp",
       generations=10, popsize=20, 
       verbose=0, logevals=TRUE)

e<-Run(penv=envXOR, grammar=BG, algorithm="sge", genemap="Mod", 
    generations=10, popsize=20, 
    reportEvalErrors=FALSE, 
    verbose=0, logevals=TRUE)

f<-Run(penv=lau15, max=FALSE, algorithm="sgperm", 
       genemap="Identity", mutation="MutateGeneMix", 
       verbose=0, logevals=TRUE)

