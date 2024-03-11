#!/bin/sh 
########## Begin Slurm header ##########
#
# Give job a reasonable name
#SBATCH --job-name=benchRmpiSlurm
#
# You have to specify a cluster partition/queue
#SBATCH --partition multiple
#
# Request number of nodes for job
#SBATCH --nodes=20
#
# Number of program instances to be executed
#SBATCH --tasks-per-node=40
#
# Maximum run time of job
#SBATCH --time=00:45:00
#
# Write standard output and errors in same file
#SBATCH --output="output-%x-%j.log"
#
#
# Send mail when job begins, aborts and ends
#SBATCH --mail-type=BEGIN,END,FAIL
#
############ End Slurm header ##########

echo "Working Directory:                    $PWD"
echo "Running on host                       `hostname`"
echo "Job id:                               $SLURM_JOB_ID"
echo "Job name:                             $SLURM_JOB_NAME"
echo "Number of nodes allocated to job:     $SLURM_NNODES"
echo "Number of cores allocated to job:     $SLURM_NTASKS"

# Setup R and Rmpi Environment
module load math/R/4.1.2
module load mpi/openmpi/4.1

mpirun Rscript XORGPRmpi.R
