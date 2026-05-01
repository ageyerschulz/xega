#/bin/sh

# number of mpi processes
NPIDS=4

# R command and profile for mpi
R_CMD="Rscript --no-save --quiet"
#R_PROFILE_USER="rmpiProfile.R"
TERMINAL_CMD="exo-open --launch TerminalEmulator --disable-server --initial-title='MPI Terminal' --color-bg='#ccc' --geometry='70x15'"
TERMINAL_EXEC_OPTION="-x"


for ((I=0; I<NPIDS; I++))
do
  $TERMINAL_CMD $TERMINAL_EXEC_OPTION $R_CMD $1 $I $NPIDS 
done

