#/bin/sh

# number of mpi processes
NPIDS=4

# R command and profile for mpi
R_CMD="Rscript --no-save --quiet"
#R_PROFILE_USER="rmpiProfile.R"

#if test -n "$R_PROFILE_USER" ; then
#        export R_PROFILE_USER
#fi

# load modules
module load mpi/openmpi-x86_64

mpirun -n $NPIDS $R_CMD $@

