#!/bin/bash

set -e
set -x

cd cpu
make ARCH=CPU -f ../makefile

for i in $(seq 10)
do
./wrap_acpcmt.x   --case ../t0031 --print --diff --diff-block-list 1 > out.txt 2>&1
if [ $i -eq 1 ]
then
  \mv -f out.txt out.txt.ref
else
diff out.txt out.txt.ref
fi
done

cd ..


cd gpu
make ARCH=GPU -f ../makefile

for i in $(seq 20)
do
./wrap_acpcmt.x   --case ../t0031 --print --diff --diff-block-list 1 > out.txt 2>&1
set +e
diff ../cpu/out.txt out.txt
c=$?
set -e
if [ $c -ne 0 ]
then
break
fi
done

cd ..


vim -d */out.txt

