#/bin/sh

# number of mpi processes
NPIDS=4

# R command and profile for mpi
R_CMD="Rscript --no-save --quiet"
#R_PROFILE_USER="rmpiProfile.R"
TERMINAL_CMD="gnome-terminal --geometry='70x15'"


for ((I=0; I<NPIDS; I++))
do
  $TERMINAL_CMD -- $R_CMD $1 $I $NPIDS 
done

