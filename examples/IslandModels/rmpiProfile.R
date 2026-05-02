# Needed by Rmpi
library(utils)
library(stats)
library(datasets)
library(grDevices)
library(graphics)
library(methods)
# Load Rmpi
library(Rmpi)

# Provide RmpiFNS for rmpi function injection to xega
RmpiFNS<-list()
RmpiFNS$mpi.send.Robj<-Rmpi::mpi.send.Robj
RmpiFNS$mpi.recv.Robj<-Rmpi::mpi.recv.Robj
RmpiFNS$mpi.iprobe<-Rmpi::mpi.iprobe
RmpiFNS$mpi.probe<-Rmpi::mpi.probe
RmpiFNS$mpi.any.source<-Rmpi::mpi.any.source
RmpiFNS$mpi.parLapply<-Rmpi::mpi.parLapply
RmpiFNS$mpi.barrier<-Rmpi::mpi.barrier

# Duplicate communicator 0 (created by default) to 
# communicator 1 (used as default communicator by mpi methods).
# I do not fully understand this.

invisible(mpi.barrier())
invisible(mpi.comm.dup(0,1))

# Only by master. And sync of slaves.
# Random stream
# invisible(RNGkind("L'Ecuyer-CMRG"))
# myRank<-mpi.comm.rank()
# if (0==myRank)  {mpi.setup.rngstream(iseed=NULL, comm=1)}

rmpi_finalize<-function() {
	if(is.loaded("mpi_initialize")) {
		mpi.finalize()
	}
}

.Last<-function() {
        rmpi_finalize()
}

log<-function(...) {
	message = paste0(...)
	cat("Rank: ", mpi.comm.rank(), ": ", message, "\n", sep="")
}

print_info<-function() {
	if (mpi.comm.rank() != 0) {
		cat("I am MPI worker no", mpi.comm.rank(), "of", mpi.comm.size(), "processes.\n")
	} else {
		cat("I am the MPI master of", mpi.comm.size(), "processes.\n")
	}
}

set_window_title<-function() {
	title=paste("MPI Rank", mpi.comm.rank())
	cat("\033]0;", title, "\007", sep="")
}

if(interactive()) {
	set_window_title()
	print_info()
}

