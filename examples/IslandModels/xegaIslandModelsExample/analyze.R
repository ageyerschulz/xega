
library(xegaDiag)

Profile<-function (s) 
{
    defaultOptions <- options()
    options(digits = 3)
    options(scipen = 999)
    opt <- c("Min", "Max")
    cat("Run time profile of:", opt[1 + s$GAenv$max], " ", s$GAenv$penv$name(), 
        "!\n")
    if (s$GAenv$profile == FALSE) {
        cat("Time used in Mainloop (in seconds):", s$timer$tMainLoop, 
            "\n")
        cat("To measure run time profile, rerun GA with option profile=TRUE.\n")
        InFunction <- list()
        TimeSpent <- vector()
        PercentageSpent <- vector()
        R <- data.frame(InFunction, TimeSpent, PercentageSpent)
        options(defaultOptions)
        invisible(R)
    }
    else {
        TimeUsed <- names(s$timer)
        Label <- list()
        TimeSpent <- vector()
        for (i in (2:length(TimeUsed))) {
            if (startsWith(TimeUsed[[i]], "t") == TRUE) {
                Label <- c(Label, substr(TimeUsed[[i]], 2, nchar(TimeUsed[[i]])))
                TimeSpent <- c(TimeSpent, s$timer[[i]])
            }
        }
        PercentageSpent <- 100 * (TimeSpent/sum(TimeSpent))
        InFunction <- unlist(Label)
        R <- data.frame(InFunction, TimeSpent, PercentageSpent)
#        print(R)
        options(defaultOptions)
        invisible(R)
    }
}


rdsCollect<-function(result=list(), lF=lF)
{
r<-list()
pat<-"*\\.rds"
pat2<-"xegaResult"
s<-0.01
j<-1
filesDone<-list()
repeat
{ fnsall<-list.files(path=lF$path(), pattern=pat)
  fns<-fnsall[grepl(pat2, fnsall)]
if (!length(fns)==0)
{
for (i in (1: length(fns)))
{  fname<-fns[[i]]
   if (fname %in% filesDone) {next}
   fn<-file.path(lF$path(), fname)
   readError<-FALSE
   tryCatch(
            {g<-readRDS(fn)},
             error=function(e) 
                     {# cat("rdsCollect: readRDS", fn, "failed.\n")
                       Sys.sleep(0.1)
                       readError<<-TRUE 
                     }
           )
   if (readError) {break}
   r[[length(r)+1]]<-g
   filesDone<-c(filesDone, fname)
#   file.remove(fn) 
    }
if (length(r)==lF$npid())
   {rc<-0; break}      
}
   Sys.sleep(s)
   j<-j+1
if ((j*s)>lF$maxDelay())
   {rc<-(-1); break} 
}
res<-list()
res$results<-r
res$rc<-rc
return(res)}

analyzeExp<-function(path=".")
{
lF<-list()
lF$path<-function() {path}
lF$maxDelay<-function() {1}
lF$npid<-function() {4}
a<-rdsCollect(result=list(), lF=lF)
b<-a$results

df<-data.frame(pid=numeric(),
               generations=numeric(),
               generationLimit=numeric(),
               shortestTour=numeric(),
               evaluations=numeric(),
               migrationTime=numeric(),
               time=numeric())

for (i in (1:length(b)))
{
s<-b[[i]]$popStat
g<-nrow(s)
col<-(1:length(colnames(s)))[colnames(s) %in% "max"]
pid<-b[[i]]$GAenv$pid
generations<-b[[i]]$GAenv$generations
popsize<-b[[i]]$GAenv$popsize
tp<-Profile(b[[i]])
tt<-sum(tp$TimeSpent)
mt<-tp$TimeSpent[5]
row<-c(pid, g, generations, s[g, col], (popsize*g), mt, tt)
df<-rbind(df, row)
cat("pid", pid, 
    "Generations", g, "(", generations,")", 
    "Shortest Tour", s[g,col], 
    "Evaluations", (popsize*g), 
     "time (s)",tt,  "\n")
}


colnames(df)<-c("pid", "Gen", "Limit", "ShortestTour", "Evals", "MigrationTime", "Time") 

pidnames<-c("2Opt_Rep_20", "kOptLK_Rep_100", "Inversion", "Greedy")

df<-cbind(df, pidnames[1+df$pid]) 
Path<-rep(path, length(b))
df<-cbind(df, Path) 

cns<-colnames(df)
cns[8]<-"Mutation"
colnames(df)<-cns

# Sort df!
sorted<-sort(df$pid, index.return=TRUE)
df<-df[sorted$ix,]

return(df)
}

cat("end of analyze.R \n")
