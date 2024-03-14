#!/bin/bash
#SBATCH --partition=single
#SBATCH --job-name=futureHPC.job
#SBATCH --output=futureHPC.output
#SBATCH --time=02:00:00
#SBATCH --mem=16GB
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=40
module load math/R/4.1.2
Rscript futureHPC.R
