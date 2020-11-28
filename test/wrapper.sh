#!/bin/bash
set -x
pwd
echo "GFORTRAN_CONVERT_UNIT="$GFORTRAN_CONVERT_UNIT
ls -1lah
echo $@
cat $1 | $2  