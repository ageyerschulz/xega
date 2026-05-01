#/bin/sh
# $1 ... R start script
NUMBER_OF_TERMINALS=4

R_CMD="R --no-save --quiet"
R_PROFILE_USER=$1

#TERMINAL_CMD="xterm"
#TERMINAL_EXEC_OPTION="-e"
TERMINAL_CMD="exo-open --launch TerminalEmulator --disable-server --initial-title='MPI Terminal' --color-bg='#ccc' --geometry='70x15'"
TERMINAL_EXEC_OPTION="-x"

if test -n "$R_PROFILE_USER" ; then
	export R_PROFILE_USER
fi

module load mpi/openmpi-x86_64

if test -n "$R_CMD" ; then
	set -- $TERMINAL_CMD $TERMINAL_EXEC_OPTION $R_CMD "$@"
else
	set -- $TERMINAL_CMD "$@"
fi

mpirun -n "$NUMBER_OF_TERMINALS" "$@"
