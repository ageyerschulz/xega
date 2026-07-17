#!/bin/sh
echo "Clean *.out files"
rm -rf ./outfile/
mkdir ./outfile
echo "Clean Directories"
./cleanDirectories.sh
echo "Build configurations"
Rscript buildTSPlin105.R
echo "For homogeneous islands (mpi)"
./mpiRun.sh mpiTSPlin105Hom.R > outfile/mpiHom.out
echo "For homogeneous islands (mpi) adaptive"
./mpiRun.sh mpiTSPlin105HomAdapt.R > outfile/mpiHomAdapt.out
echo "For heterogeneous islands (mpi)"
./mpiRun.sh mpiTSPlin105Het.R > outfile/mpiHet.out
echo "For heterogeneous islands (mpi) adaptive"
./mpiRun.sh mpiTSPlin105HetAdapt.R > outfile/mpiHetAdapt.out
echo "For heterogeneous islands (mpi) adaptive optimistic"
./mpiRun.sh mpiTSPlin105HetAdaptOptimist.R > outfile/mpiHetAdaptOptimist.out
echo "For heterogeneous islands (mpi) early termination"
./mpiRun.sh mpiTSPlin105HetEarly.R > outfile/mpiHetEarly.out
echo "For heterogeneous islands (mpi) barrier synchronization"
./mpiRun.sh mpibTSPlin105Het.R > outfile/mpibHet.out
echo "mpi examples done."

echo "Clean Directories"
./cleanDirectories.sh

echo "For homogeneous islands (rds)"
./rdsRun.sh rdsTSPlin105Hom.R > outfile/rdsHom.out
echo "For homogeneous islands (rds) Collect"
./cleanDirectories.sh
./rdsRun.sh rdsTSPlin105HomCollect.R > outfile/rdsHomCollect.out
wait
echo "For heterogeneous islands (rds)"
./cleanDirectories.sh
./rdsRun.sh rdsTSPlin105Het.R > outfile/rdsHet.out
echo "For heterogeneous islands (rds) adaptive"
./cleanDirectories.sh
./rdsRun.sh rdsTSPlin105HetAdapt.R > outfile/rdsHetAdapt.out
echo "For heterogeneous islands (rds) early termination"
./cleanDirectories.sh
./rdsRun.sh rdsTSPlin105HetEarly.R > outfile/rdsHetEarly.out
echo "For heterogeneous islands (rds) barrier synchronization"
./cleanDirectories.sh
./rdsRun.sh rdsbTSPlin105HetAdapt.R > outfile/rdsbHetAdapt.out
echo "rds examples done."
