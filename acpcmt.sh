#!/bin/bash
#SBATCH --export=NONE
#SBATCH --nodes=1
#SBATCH --gres=gpu:1
#SBATCH --partition=gpu_p2,gpu_p1
#SBATCH --time 00:05:00
#SBATCH --exclusive

module load nvidia-compilers/20.11

set -x
ulimit -s unlimited

nsys=$(which nsys)


cd /gpfswork/rech/jau/ufh62jk/acpcmt/openacc

NPROF=0
GIT=$(./git.pl)

if [ "$NPROF" -eq 0 ]
then

srun -n 1 $nsys profile --force-overwrite true -t openacc,nvtx,osrt,cuda -o acpcmt.$GIT.prof \
./wrap_acpcmt.x --case ../t1198 --times 2

nsys stats ./acpcmt.$GIT.prof.qdrep


elif [ "$NPROF" -eq 1 ]
then

nvprof --print-gpu-trace --log-file nvprof.txt \
./wrap_acpcmt.x --case ../t1198 
cat nvprof.txt

elif [ "$NPROF" -eq 2 ]
then

#ncu --set full  -f -o acpcmt.prof \
#./wrap_acpcmt.x --case ../t1198

ncu --set full  -f -o acpcmt.prof \
./wrap_acpcmt.x --case t0031

else

export NVCOMPILER_ACC_NOTIFY=31
./wrap_acpcmt.x --case t0031 --diff > acpcmt.eo 2>&1

fi




