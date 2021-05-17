#!/bin/bash
#SBATCH --export=NONE
#SBATCH --nodes=1
#SBATCH --gres=gpu:1
#SBATCH --partition=gpu_p2,gpu_p1
#SBATCH --time 00:05:00
#SBATCH --exclusive

set -x
ulimit -s unlimited

module load nvidia-compilers/20.11


cd /gpfswork/rech/jau/ufh62jk/acpcmt/openacc

NPROF=0

if [ "$NPROF" -eq 0 ]
then

nsys profile --force-overwrite true -t openacc,nvtx,osrt,cuda -o acpcmt.prof \
./wrap_acpcmt.x --case ../t1198

nsys stats acpcmt.prof.qdrep


elif [ "$NPROF" -eq 1 ]
then

nvprof --print-gpu-trace --log-file nvprof.txt \
./wrap_acpcmt.x --case t0031 --diff > acpcmt.eo 2>&1
cat nvprof.txt

else

export NVCOMPILER_ACC_NOTIFY=31
./wrap_acpcmt.x --case t0031 --diff > acpcmt.eo 2>&1

fi

cat acpcmt.eo



