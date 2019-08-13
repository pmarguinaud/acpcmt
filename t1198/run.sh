#!/bin/bash
#SBATCH -N1
#SBATCH --time 00:05:00
#SBATCH --exclusive
#SBATCH -p normal64,huge256
#SBATCH --export="NONE"

set -x

cd /home/gmap/mrpm/marguina/pack/42_acpcmt.02.I161150INTELMPI512150MT.x/test/t1198

ulimit -s unlimited
export OMP_STACK_SIZE=2G

../wrap_acpcmt.x

