#!/bin/bash

set -x

cd noacc
for f in *.F90
do
  ../openacc.pl $f
  mv $f.new ../$f
done
