#/bin/sh
# $1 ... R start script
NUMBER_OF_TERMINALS=4

R_PROFILE_USER=$1

if test -n "$R_PROFILE_USER" ; then
	export R_PROFILE_USER
fi

module load mpi/openmpi-x86_64

mpirun -n "$NUMBER_OF_TERMINALS" xfce4-terminal --disable-server --color-bg='#333' --geometry='70x15' -x R --no-save --quiet
