#!/bin/sh
NUM_JOBS=8
module purge
module load mpi/openmpi-x86_64
mpirun -N $NUM_JOBS --host localhost:8 Rscript XORGPRmpi.R > XORGPRmpi.out
