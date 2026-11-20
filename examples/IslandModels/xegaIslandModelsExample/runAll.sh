#!/bin/sh
./cleanDirectories.sh
Rscript buildTSPlin105.R
echo "buildTSPlin105.R: Done"
./mpiRun.sh mpiTSPlin105Het.R
echo "mpiTSPlin105Het.R: Done"
./mpiRun.sh mpibTSPlin105Het.R
echo "mpibTSPlin105Het.R: Done"
./mpiRun.sh mpiTSPlin105HetAdapt.R
echo "mpiTSPlin105HetAdapt.R: Done"
./mpiRun.sh mpiTSPlin105HetAdaptOptimist.R
echo "mpiTSPlin105HetAdaptOptimist.R: Done"
echo "Done"
