#!/bin/bash
#SBATCH --partition=single
#SBATCH --job-name=GPXORmultiCore.job
#SBATCH --output=GPXORmultiCore.output
#SBATCH --time=00:15:00
#SBATCH --mem=16GB
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=40
module load math/R/4.1.2
Rscript GPXORmultiCore.R
