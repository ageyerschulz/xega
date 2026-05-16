
library(xega)

b1<-xegaRun(penv=Parabola2D, algorithm="sga", 
   generations=50, popsize=100, max=FALSE, 
   replication="Kid2Pipeline", crossover="Cross2Gene", pipeline="PipeC",
   terminationCondition="LEQ", terminationThreshold=1.0, 
   verbose=2, replay=5, profile=TRUE, debug=TRUE)

cat("b1 done.\n")

b2<-xegaRun(penv=Parabola2D, algorithm="sga", 
   generations=50, popsize=100, max=FALSE, 
   replication="Kid2Pipeline", crossover="Cross2Gene", pipeline="PipeC",
   terminationCondition="LEQ", terminationThreshold=0.04, 
   verbose=2, replay=5, profile=TRUE, debug=FALSE)

cat("b2 done.\n")

